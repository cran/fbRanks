plot.fbRanks=function(x,...,which="residuals",annotate=list(title=TRUE)){
  if(length(which)>1) devAskNewPage(TRUE)
  if(which=="residuals"){
    extra=list(...)
    team.names=extra$Name
    team.resids = residuals(x)
    if(identical(team.names,"all")) team.names = names(team.resids)
    if(!all(team.names %in% names(team.resids))){
      cat("The following team names are not recognized and will be ignored:\n")
      cat(team.names[!(team.names %in% names(team.resids))])
      cat("\n")
      team.names = team.names[(team.names %in% names(team.resids))]
    }
    for(i in team.names){
    if(dim(team.resids[[i]])[1]==0) next
    ylim=c(min(-3,team.resids[[i]]$attack.residuals-.5,-1*team.resids[[i]]$defense.residuals-.5,na.rm=TRUE),
           max(3,team.resids[[i]]$attack.residuals+.5,-1*team.resids[[i]]$defense.residuals+.5,na.rm=TRUE))
    xlim=c(min(team.resids[[i]]$date),min(max(team.resids[[i]]$date),as.Date(x$max.date, x$date.format)))
    ylim=c(-6,6)
    cat("Actual-Predicted Residuals Plot\n")
    if(x$time.weight.eta!=0) cat("Model is time-weighted. A model optimized for current date is being used to predict past dates.\n")
    plot(team.resids[[i]]$date,team.resids[[i]]$attack.residuals,xlab="",
         ylab="Actual versus Predicted Goals",
         xlim=xlim,ylim=ylim)
    points(team.resids[[i]]$date,-1*team.resids[[i]]$defense.residuals,col="red",pch=2)
    
    game.dates=team.resids[[i]]$date
    game.resids=-1*team.resids[[i]]$defense.residuals
    bad=is.na(game.resids)
    if(length(game.dates[!bad])>10){ #add the time line
    x=game.dates[!bad]
    y=game.resids[!bad]
    lo <- loess(y~as.numeric(x),degree=2)
    #plot(x,y,ylim=c(-4,4),col="red",cex=.5)
    lines(x,predict(lo), col='red', lwd=2)
    game.dates=team.resids[[i]]$date
    game.resids=team.resids[[i]]$attack.residuals
    bad=is.na(game.resids)
    x=game.dates[!bad]
    y=game.resids[!bad]  
    lo <- loess(y~as.numeric(x),degree=2)
    #points(x,y,ylim=c(-4,4),col="blue",pch=2,cex=.5)
    lines(x,predict(lo), col='blue', lwd=2)
    #abline(h=0,lty=3)
    }
    
    abline(h=0)
    abline(h=c(-1,1),lty=2)
    legend("bottomright",c("Attack","Defense"),col=c("blue","red"),pch=c(1,2),cex=1,horiz=TRUE,lty=c(1,1))
    title(paste(i,"\nDots are actual minus expected GF (blue circles) and GA (red triangles).\nBlue line is smoothed change in attack strength. Red is smoothed change in defense strength.",sep=""))
    text(xlim[1],ylim[1]+.1,"Below 0 indicates poorer than expected", pos=4,cex=.7)
    text(xlim[1],ylim[2]-.1,"Above 0 indicates better than expected", pos=4,cex=.7)
    }
  }
  if(substr(which,1,4)=="hist"){
    team.names=annotate$Name
    team.ranks=print(x,...,silent=TRUE)
    hist.type=strsplit(which,"\\.")[[1]][2]
    if(hist.type=="total") dat=team.ranks$ranks$total
    if(hist.type=="attack") dat=log(team.ranks$ranks$attack)
    if(hist.type=="defense") dat=log(team.ranks$ranks$defense)
    hist(dat,main="",xlab="")
    if(identical(annotate[["title"]],TRUE) | is.null(annotate[["title"]])) title(paste("team",strsplit(which,"\\.")[[1]][2],"strength (red line)\nrelative to all other teams (histogram)"))
    else title(annotate[["title"]])
      if(!is.null(team.names)){
      if(identical(team.names,"all")) team.names = team.ranks$ranks$team
    if(!all(team.names %in% team.ranks$ranks$team)){
      cat("The following team names are not recognized and will be ignored:\n")
      cat(team.names[!(team.names %in% team.ranks$ranks$team)])
      cat("\n")
      team.names = team.names[(team.names %in% team.ranks$ranks$team)]
    }
    }
    for(i in team.names){
      abval=team.ranks$ranks[[hist.type]][team.ranks$ranks$team==i]
      if(hist.type!="total") abval=log(abval)
      abline(v=abval,col="red",lwd=2)
    }
  }
  if(substr(which,1,11)=="strength.by"){
    extra=list(...)
    bytime=strsplit(which,"\\.")[[1]][3]
    team.names=extra$Name
    cat(paste("Strength by",bytime,"(This takes awhile)\n"))
        
    team.resids = residuals(x)
    if(identical(team.names,"all")) team.names = names(team.resids)
    if(!all(team.names %in% names(team.resids))){
      cat("The following team names are not recognized and will be ignored:\n")
      cat(team.names[!(team.names %in% names(team.resids))])
      cat("\n")
      team.names = team.names[(team.names %in% names(team.resids))]
    }
    for(i in team.names){
      if(dim(team.resids[[i]])[1]==0) next
      ylim=c(min(-3,team.resids[[i]]$attack.residuals-.5,-1*team.resids[[i]]$defense.residuals-.5,na.rm=TRUE),
             max(3,team.resids[[i]]$attack.residuals+.5,-1*team.resids[[i]]$defense.residuals+.5,na.rm=TRUE))
      xlim=c(min(team.resids[[i]]$date),min(max(team.resids[[i]]$date),as.Date(x$max.date, x$date.format)))
      cat("Actual-Predicted Residuals Plot\n")
      if(x$time.weight.eta!=0) cat("Model is time-weighted. A model optimized for current date is being used to predict past dates.\n")
      plot(team.resids[[i]]$date,team.resids[[i]]$attack.residuals,xlab="",
           ylab="Actual versus Predicted Goals",
           xlim=xlim,ylim=ylim)
      points(team.resids[[i]]$date,-1*team.resids[[i]]$defense.residuals,col="red",pch=2)
      abline(h=0)
      abline(h=c(-1,1),lty=2)
      legend("bottomright",c("Attack","Defense"),col=c("black","red"),pch=c(1,2),cex=.8,horiz=TRUE)
      title(i)
      text(xlim[1],ylim[1]+.1,"Below 0 indicates poorer than expected", pos=4,cex=.7)
      text(xlim[1],ylim[2]-.1,"Above 0 indicates better than expected", pos=4,cex=.7)
    }
  }
  devAskNewPage(FALSE)
}