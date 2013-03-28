rank.teams=function(scores=NULL, teams=NULL, 
                    family="poisson", fun="glm",
                    max.date="2100-6-1", min.date="1900-5-1", date.format="%Y-%m-%d",
                    intercept=FALSE,
                    time.weight.eta=0,
                    add=NULL, silent=FALSE, ...){
#g(mu)=log(mu)
#score(i,j) ~ Pois(alpha_i * beta_j); i is self, j is opposing team
#x_i, the self team, a 1 x # teams matrix with 1 in i-th column, 0 otherwise
#x_j, the opposing team, a 1 x # teams matrix with 1 in j-th column, 0 otherwise
#glm(formula = score.i.vs.j ~ factor(attack.i) + factor(defense.j), data=scores, family = family)
require(igraph)
require(stringr)

#do some error checking on the scores data frame
if(missing(scores)) stop("A scores data frame must be passed in.\n",call.=FALSE)
if(!is.data.frame(scores)) stop("scores must be a data frame.\n",call.=FALSE)
colnames(scores)=tolower(str_strip.white(names(scores)))
if(any(duplicated(names(scores))))
  stop("Duplicated column names are not allowed in the score data frame.  Names are case insensitive and extra space is striped.\n", call.=FALSE)
req.columns=c("date","home.team","home.score","away.team","away.score")
if(!all(req.columns %in% names(scores)))
  stop("The scores data frame must include the columns: date, home.team, home.score, away.team, away.score\n", call.=FALSE)
#no factors allowed
for(i in names(scores)){
  if(is.factor(scores[[i]])) scores[[i]]=as.character(scores[[i]])
}
if(any(is.na(as.Date(scores$date,date.format))))
  stop("The scores dates are not in the same format as date.format.\n",call.=FALSE)

#do some error checking on the teams data frame
if(missing(teams)){
  if(!silent) cat("Alert: teams data frame was not passed in. Will attempt to construct one from the scores data frame.You should ensure that teams use only one name in scores data frame.\n")
  teams=data.frame(name=sort(unique(c(scores$home.team,scores$away.team))),stringsAsFactors=FALSE)
}
if(!is.data.frame(teams)) stop("teams must be a data frame.\n",call.=FALSE)
colnames(teams)=tolower(str_strip.white(names(teams)))
if(any(duplicated(names(teams))))
  stop("Duplicated column names are not allowed in the teams data frame.  Names are case insensitive and extra space is striped.\n", call.=FALSE)
req.columns=c("name")
if(!all(req.columns %in% names(teams)))
  stop("The teams data frame must include the column \"name\" which is the team name.\n", call.=FALSE)
#no factors allowed
for(i in names(teams)){
  if(is.factor(teams[[i]])) teams[[i]]=as.character(teams[[i]])
}

#all column names in scores and teams must be unique
if(any(duplicated(c(names(scores),names(teams)))))
  stop("Column names that appear in both the scores and teams data frames are not allowed.  Names are case insensitive and extra space is striped.\n", call.=FALSE)

#Check for mis-entered years
if(is.na(as.Date(max.date, date.format))) stop(paste("max.date must be entered in the following format:",format(Sys.Date(),date.format),"\n or pass in the argument date.format to specify a different date format.\n"))
if(is.na(as.Date(min.date, date.format))) stop(paste("min.date must be entered in the following format:",format(Sys.Date(),date.format),"\n or pass in the argument date.format to specify a different date format.\n"))
#store dates as Date objects
max.date = as.Date(max.date, date.format)
min.date = as.Date(min.date, date.format)

#Error check on the team names in scores and in the team file
display.names=unique(teams$name)
nteams = length(unique(display.names))

ok=TRUE
#Check for bad names
bad.names=scores$home.team[!(scores$home.team %in% display.names)]
if(length(bad.names)!=0){
  cat("The following home team names don't match names in team file:\n")
  cat(as.character(bad.names),sep=", ")
  cat("\nerrors are on lines ")
  cat((1:dim(scores)[1])[!(scores$home.team %in% display.names)]); cat("\n")
  ok=FALSE
}
bad.names=scores$away.team[!(scores$away.team %in% display.names)]
if(length(bad.names)!=0){
  cat("The following away team names don't match names in team file:\n")
  cat(as.character(bad.names),sep=", ")
  cat("\nerrors are on lines ")
  cat((1:dim(scores)[1])[!(scores$away.team %in% display.names)]); cat("\n")
  ok=FALSE
}
if(!ok){ 
  cat("Bad team names in scores file.  Score file being returned.\n")
  return(scores=scores)
}

#determine which teams to include based on any filter arguments the user included
tmp=team.and.score.filters(list(scores=scores, teams=teams),...)
scores=scores[tmp$include.scores,]
teams=teams[teams$name %in% tmp$include.teams,]

#Check that any names in add.to.formula appears in scores
if(!is.null(add) & !is.character(add))
  stop("add must be a vector of names to add to the model.",call.=FALSE)
scores.names= names(scores)[!(names(scores) %in% c("date","home.team","home.score","away.team","away.score"))]
add.to.formula=list()
for(tmp in add){
  if(paste("home.",tmp,sep="") %in% scores.names){
    if(!(paste("away.",tmp,sep="") %in% scores.names)) stop(paste("home.",tmp," appears in scores file, therefore, away.",tmp," must also appear.\n", sep=""), call.=FALSE)
    if(tmp %in% scores.names) stop(paste("home.",tmp, ", home.",tmp, " and ", tmp, "appear in scores file, therefore the predictor to add is ambiguous.\n", sep=""), call.=FALSE)
    }
  if(paste("away.",tmp,sep="") %in% scores.names){
    if(!(paste("home.",tmp,sep="") %in% scores.names)) stop(paste("away.",tmp," appears in scores file, therefore, home.",tmp," must also appear.\n", sep=""), call.=FALSE)
    }
  if(str_sub(tmp,1,5)=="home.")
    stop(paste(tmp, "should not have the home. prefix.  Predictors that are different for home and away are\nentered as home.pred and away.pred (a pair) in scores file, and only pred is passed into add.to.formula.\n", sep=""), call.=FALSE)
  if(str_sub(tmp,1,5)=="away.")
    stop(paste(tmp, "should not have the away. prefix.  Predictors that are different for home and away are\nentered as home.pred and away.pred (a pair) in scores file, and only pred is passed into add.to.formula.\n", sep=""), call.=FALSE)
if(paste("home.",tmp,sep="") %in% scores.names){
  add.to.formula[[tmp]]=c(paste("home.",tmp,sep=""),paste("away.",tmp,sep=""))
}else{
  if(!(tmp %in% scores.names))
    stop(paste(tmp, " is in add but no column of that name in the scores file.\n", sep=""), call.=FALSE)
  add.to.formula[[tmp]]=c(tmp,tmp)  
}
}

#Replace teams with a data.frame with only the teams in scores

#Set up the data to be used for the ranking
scores.glm = scores

#Set up the level names for the home and away teams
level.names = sort(unique(c(as.character(scores$home.team),as.character(scores$away.team))))
scores.glm$home.team=factor(scores$home.team, levels=level.names)   
scores.glm$away.team=factor(scores$away.team, levels=level.names)   

#subset scores to date range
scores.glm = scores.glm[scores.glm$date>=min.date,]
scores.glm = scores.glm[scores.glm$date<=max.date,]

#Set up the factor names for glm
attack.team=c(as.character(scores.glm$home.team),as.character(scores.glm$away.team))
defense.team=c(as.character(scores.glm$away.team),as.character(scores.glm$home.team))
scores.team=c(scores.glm$home.score,scores.glm$away.score)
attack=factor(attack.team, levels=level.names)
defend=factor(defense.team, levels=level.names)

# Detect any unconnected clusters and rank them as separate groups
el=cbind(as.character(scores.glm$home.team),as.character(scores.glm$away.team))
g1 = graph.edgelist(el,directed=FALSE)
clus=clusters(g1) #by factor order
clus.names = get.vertex.attribute(g1, "name")

glm.fit=list()
lmer.fit=list()
#set up the residuals column for scores
scores$home.residuals=NA
scores$away.residuals=NA
el.full=cbind(as.character(scores$home.team),as.character(scores$away.team))
date.filter = scores$date>=min.date & scores$date<=max.date

 tmp.fun = function(x,y){ any(x %in% y) }
 nlevels.all = nlevels(attack)
 for(cluster in 1:clus$no){
   names.in.clus = clus.names[clus$membership == cluster]
   #el is a 2 column matrix with home.team in col 1 and away.team in col 2
   rows.clus = apply(el,1,tmp.fun,names.in.clus)
   scores.clus = scores.glm[rows.clus,]
   time.diff=as.numeric(max.date-scores.clus$date)
   time.diff=c(time.diff,time.diff) #since away and home
   attack.team=c(as.character(scores.clus$home.team),as.character(scores.clus$away.team))
   defense.team=c(as.character(scores.clus$away.team),as.character(scores.clus$home.team))
   scores.team=c(scores.clus$home.score,scores.clus$away.score)
   attack=factor(attack.team)
   defend=factor(defense.team)

   #Now add any addition factors to the model
   add.to.f=""
   for(add.f in names(add.to.formula)){
     ok=TRUE
     new.col=c(scores.clus[[add.to.formula[[add.f]][1]]], scores.clus[[add.to.formula[[add.f]][2]]])
     if(is.character(new.col)){
       new.col=factor(new.col)
       if(length(levels(new.col))==1){
         ok=FALSE #don't add that factor to the fit
       }
     }
     assign(paste(add.f,".f",sep=""), new.col)
     if(ok) add.to.f=paste(add.to.f,"+",paste(add.f,".f",sep=""),sep="")
   }

   if("glm" %in% fun){
     #set up the formula
     my.formula = paste("scores.team~attack+defend",add.to.f,sep="")
     if(!intercept) my.formula=paste(my.formula,"-1",sep="")
     glm.fit[[paste("cluster.",cluster,sep="")]]=
       glm(formula=as.formula(my.formula), family=family, na.action="na.exclude",weights=exp(-1*time.weight.eta*time.diff))
   }
   if("lmer" %in% fun){
     require(lme4)
     #set up the formula
     my.formula = paste("scores.team~(1|attack)+(1|defend)",add.to.f,sep="")
     if(!intercept) my.formula=paste(my.formula,"-1",sep="")
     lmer.fit[[paste("cluster.",cluster,sep="")]]=
       lmer(formula=as.formula(my.formula), family=family)
    #to get effects use ranef(glm.fit)$attack and ranef$defend
   }
   
   #which rows in the full scores, with all dates, are in the glm cluster; rows.clus.full is needed to subset scores
   rows.clus.full = apply(el.full,1,tmp.fun,names.in.clus) & date.filter
   theresids=residuals(glm.fit[[paste("cluster.",cluster,sep="")]],type="response")
   scores$home.residuals[rows.clus.full]=theresids[1:(length(theresids)/2)]
   scores$away.residuals[rows.clus.full]=theresids[(length(theresids)/2+1):length(theresids)]
 }


# Set up the fbRanks object to return
rtn.list = list(fit=glm.fit, lmer=lmer.fit, graph=list(graph=g1, membership=clus$membership, csize=clus$csize, no=clus$no, names=get.vertex.attribute(g1, "name")), 
                scores=scores, teams=teams,
                max.date=max.date, min.date=min.date,time.weight.eta=0, date.format=date.format)
class(rtn.list) = "fbRanks"

if(!silent) print(rtn.list)

invisible(rtn.list)
}

