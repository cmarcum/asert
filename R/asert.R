asert<-function(x){
 this.env<-environment()
	onOkay<-function(){
	  cvars<-as.character(tkcurselection(np.selectlist))
	  assign(paste("nv",get("i",this.env),sep="."),as.numeric(cvars)+1,envir=this.env)
	  tkdestroy(np)
	}
       aplen<-min(nrow(x),25)
       cwidth<-max(nchar(apply(x,1,paste,collapse=" ")))+10
       geoxy<-"1250x500+100+100"
	for(i in 1:nrow(x)){
                assign(paste("nv",get("i",this.env),sep="."),0,envir=this.env)
	   }
	for(i in 1:nrow(x)){
		if(!i%in%unlist(mget(paste("nv",1:nrow(x),sep="."),envir=this.env))){
		np<-tktoplevel()
		tkwm.geometry(np,geoxy)
                np.vals<-tclVar(apply(x,1,paste,collapse=" "))
		np.main<- tklabel(np, text = paste("Please select each person that matches: \n \n", paste(x[i,],collapse=" "),". \n \n \n To deselect, unclick or use `alt+leftclick`",collapse=" "),fg="blue",font="serif")
		np.frame<-tkframe(np)
		np.ok<-tkbutton(np,text="--OK--",command=onOkay,fg="green",bg="black",pady=5,padx=5,width=25)
                np.space<-tklabel(np,text="")
                tkgrid(np.space,row=1,columnspan=2)
		tkgrid(np.main,column=1,row=2)
		tkgrid(np.ok,column=1,row=3)
                tkgrid(np.frame,column=2,row=2,rowspan=2)
		np.sl.sb<-tkscrollbar(np.frame,orient="vertical",repeatinterval=4,command=function(...) tkyview(np.selectlist,...))
		np.selectlist<-tklistbox(np.frame,selectmode="multiple",listvariable=np.vals,width=cwidth,height=aplen,yscrollcommand=function(...) tkset(np.sl.sb,...),bg="lightblue",font="sans")
                for(k in seq(1,nrow(x),by=2)){
	                tkitemconfigure(np.selectlist,k-1,bg="lightgreen")
		}
                tkgrid(np.sl.sb,np.selectlist)
                tkgrid.configure(np.sl.sb,rowspan=4,sticky="nsw")
		tkwait.window(np)
		}
		}
	x.cands<-mget(paste("nv",1:nrow(x),sep="."),envir=this.env)
	same.ids<-rep(0,nrow(x))
	for(p in 1:length(x.cands)){
		if(x.cands[[p]][1]!=0){
			same.ids[x.cands[[p]]]<-p
			}
		}
	return(same.ids)
	}
