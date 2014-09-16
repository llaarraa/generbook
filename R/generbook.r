#file that contains all the generbook R functions
#updated 24th of September 2012

#version that includes only the accepted papers
#version that does not use the numbers or underlining if there is just 1 author
#version that writes down the affiliation of the authors just once if it is the same for all the authors

#file that contains all the generbook R functions
#updated 11th of September 2012

#version that includes only the accepted papers
#version that does not use the numbers or underlining if there is just 1 author
#version that writes down the affiliation of the authors just once if it is the same for all the authors



#' Function that generates a separate TeX file for each of the abstracts
#' @param my.filename tab delimited database that contains the abstract submission (contribution data base), full path must be given or it must be stored in the working directory of R
#' @param dirAbstracts directory where the abstracts will be stored, it must exist
#' @param author.lastname columns that contain the last name of the authors in the contribution data base (numbers must be gives, i.e. if only two authors are allowed and the last names are stored in the 5th and 10th column of the contribution database, author.lastname=c(5,10)) 
#' @param author.firstname columns that contain thefirst name of the authors
#' @param author.institution columns that contain the institution of the authors
#' @param author.city columns that contain the city of the authors
#' @param author.country columns that contain the country of the authors
#' @param author.email columns that contain the e-mails of the authors
#' @param author.presenting columns that contain an indicator T/F indicating if the author will present the work
#' @param pres.title column containing the title of the presentation
#' @param pres.abstract column containing the abstract text
#' @param accept column that contains the acceptance decision, must be (exaclty) "Yes" for the accepted abstracts
#' @param topic1 column that contains the first selected topic
#' @param topic2 column that contains the second selected topic
#' @param id column that contains the abstract ID
#' @param notes column that contains the notes added by the reviewers
#' @param ref2 opinion of reviewer2
#' @param ref3 opinion of reviewer3
#' @param ref4 opinion of reviewer4
#' @param accept.all if set to TRUE, all the abstracts included in the database are "accepted", useful to generate the first draft of the abstract book
#' @param noNotes don't write notes, set to TRUE if notes should be omitted, useful for the version published for authors
#' @param style "AS2012" (used for the book of abstracts of Applied Statistics 2012) or "AS2011" (used for the book of abstracts of Applied Statistics 2011), the styles differ in how the e-mails and affiliations of the authors are formatted
#' @param notesDay column that contains the notes of the organizers about the (possible) day of presentation, 
#' @param notesPayment column that contains the notes of the organizers about the payment/registration information about the presentation
#' @param verbose FALSE: indicates if some output should be written on screen while executing the function
#' @return TeX files are generated and stored dirAbstracts directory, My.data: data set with submitted data; PA: index indicating the presenting author for each submission.
#' @export
#' @examples 
#' set.seed(1)
 



generate.abstracts<-function(my.filename, 
						dirAbstracts,
						author.lastname=seq(23, by=6, length.out=7),
						author.firstname=seq(24, by=6, length.out=7),
						author.institution=seq(26, by=6, length.out=7),
						author.city=seq(27, by=6, length.out=7),
						author.country=seq(3, by=1, length.out=7)+1,
						author.email=seq(25, by=6, length.out=7),
						author.presenting=seq(22, by=6, length.out=7),
						pres.title=18,
						pres.abstract=21,
						accept=17,
						topic1=19,
						topic2=3,
						id=2,
						notes=73,
						ref2=77,
						ref3=78, 
						ref4=79,
						duplicated=NULL, 
						accept.all=F, #added May2011, 
						noNotes=F, 
						style="AS2012",
						notesDay=81,
						notesPayment=82,
						verbose=FALSE 
            ){


#my.filename: tab delimited database that contains the abstract submission (contribution data base), full path must be given or it must be stored in the working directory of R
#dirAbstracts: directory where the abstracts will be stored, it must exist
#author.lastname: columns that contain the last name of the authors in the contribution data base (numbers must be gives, i.e. if only two authors are allowed and the last names are stored in the 5th and 10th column of the contribution database, author.lastname=c(5,10)) 
#author.firstname: columns that contain thefirst name of the authors
#author.institution: columns that contain the institution of the authors
#author.city: columns that contain the city of the authors
#author.country: columns that contain the country of the authors
#author.email: columns that contain the e-mails of the authors
#author.presenting: columns that contain an indicator T/F indicating if the author will present the work
#pres.title: column containing the title of the presentation
#pres.abstract: column containing the abstract text
#accept: column that contains the acceptance decision, must be (exaclty) "Yes" for the accepted abstracts
#topic1: column that contains the first selected topic
#topic2: column that contains the second selected topic
#id: column that contains the abstract ID
#notes: column that contains the notes added by the reviewers
#ref2: opinion of reviewer2
#ref3: opinion of reviewer3
#ref4: opinion of reviewer4
#accept.all: if set to TRUE, all the abstracts included in the database are "accepted", useful to generate the first draft of the abstract book
#noNotes: don't write notes, set to TRUE if notes should be omitted, useful for the version published for authors
#style="AS2012" (used for the book of abstracts of Applied Statistics 2012) or "AS2011" (used for the book of abstracts of Applied Statistics 2011), the styles differ in how the e-mails and affiliations of the authors are formatted
#notesDay=column that contains the notes of the organizers about the (possible) day of presentation, 
#notesPayment=column that contains the notes of the organizers about the payment/registration information about the presentation
#verbose=FALSE: indicates if some output should be written on screen while executing the function
				
		#save the working direcory
		initial.wd=getwd()

	
	
	
	
	#read data
	my.data<-read.delim(my.filename, sep="\t")

  
  
	#set the working directory as the directory where the LaTeX files containing the abstracts will be written
	setwd(dirAbstracts)

	#calculate the number of authors for each abstract
	num.authors<-apply(my.data[,author.lastname], 1, function(x) sum(!is.na(x) & x!=""))

	#remove the empty records
	my.data<-my.data[num.authors!=0,]

  
  #change: 26/6/2014
  ########### substitute _ with \\_ in the mail addresses, in this way there is no need to change it manually in the db 
	#my.data[, author.email]=sub(pattern="_", replacement="\\\\_",  my.data[,author.email])
	my.data[, author.email]=apply(my.data[,author.email], 2, function(x) as.character(x))

	
  
  for(i in 1:nrow(my.data))
      for(j in 1:length(author.email))
        my.data[i, author.email[j]]=sub(pattern="_", replacement="\\\\_",  my.data[i,author.email[j]])
  
  
	###my.data[6, author.email[2]]=sub(pattern="_", replacement="\\\\_",  my.data[6,author.email[2]])
	
	
  
  
  
	#remove duplicated records and rejected papers, accepted paper must be indicated with "Yes" in the accept column, unless a full list is required, with the argument accept.all=T
	if(accept.all!=T) my.data<-my.data[my.data[,accept]=="Yes",]

	#calculate the number of authors
	num.authors<-apply(my.data[,author.lastname], 1, function(x) sum(!is.na(x) & x!=""))
	#calculate which is the presenting author, select just one if more than one was indicated as presenting
	presenting.author<-as.numeric(unlist(apply(my.data[,author.presenting], 1, function(x) which(x=="Yes")[1])))
	#the first author is set as the presenting author if none was indicated as presenting
	presenting.author[is.na(presenting.author)]<-1

	#initialization of vectors
	titolo=	autori = affiliazioni = abstract = rep("", dim(my.data)[1])
	
	#checking the style choosen
	if(style=="AS2012"){

			for(i in 1:dim(my.data)[1]){

				if(verbose==TRUE) cat("ID=", i)

				##########title##################################
				titolo[i]<-paste("{", my.data[i,pres.title]     , "}", sep="")
				###########end title#################################


				########### authors names##############################
				#getting author's initials
				TMP<-strsplit(as.character(my.data[,author.firstname[1]]), c(" "))
				#number of different names in first name
				number.of.names<- unlist(lapply(TMP, length))
				#derive initials of first names, used in the index
				authors.initials<-toupper(strsplit(TMP[[i]][1], "")[[1]][1]) 
				if(number.of.names[i]>1) for(j in 2:number.of.names[i])
				authors.initials<-paste(authors.initials, toupper(strsplit(TMP[[i]][j], "")[[1]][1]), sep="") 

				#determine if all the authors come from just one instutution, true if it is the same for all authors, false otherwise
				#will be used to write the insitution just once if TRUE
				
				
				
				########### added June12, to index differently the institution of the authors, to achieve for example: au1^1, au2^2, au3^1 if the authors come from A, B, A
				# the indexes to be used are stored in the vector: index.inst
				
				#vector with the institution/city of the authors of the i-th abstract
				inst.city=paste(unlist((my.data[i,author.institution[1:num.authors[i]]])), unlist((my.data[i,author.city[1:num.authors[i]]])))
				
				#one.inst.true<- length(unique(unlist((my.data[i,author.institution[1:num.authors[i]]]))))==1 & length(unique(unlist((my.data[i,author.city[1:num.authors[i]]]))))==1
				
				#how many different institutions for i-th abstract
				how.many.inst=length(unique(inst.city))
				
				#indicator one.inst.true=T if only one institution is indicated for all authors
				one.inst.true = how.many.inst==1
				#all.inst.diff.true=how.many.inst==num.authors[i]

				
				#index.inst: vector containing the index of the institution to be used for each author, it says for each author which is the (index of the) FIRST author with the same institution that appears
				if(one.inst.true) index.inst=rep(1, num.authors[i]) else {
							index.inst=as.numeric(factor(inst.city, levels=unique(inst.city))) }
				############### end added June12
				
				
        
        
        
        
        
				#derive a string containing the  author names,  as they will appear in the abstract
				#added a different way to report 1 single author, no underlying and no number 1
				
				#one author only
				if(num.authors[i]==1) {tmp<-paste(my.data[i, author.firstname[1] ], " ", my.data[i, author.lastname[1] ], 
										#"\\index{", my.data[i, author.lastname[1] ], ", ", authors.initials,   "}", 
										.fun.get.index.slo( my.data[i, author.lastname[1]] , authors.initials),   sep="") 
										#mail of the single author
										tmp.mails=paste("\\Email{", ifelse(!is.na(my.data[i, author.email[1] ]),
                                                       paste(my.data[i, author.email[1] ]),"") , "}\n", sep="")  
										                                   #paste(sub(pattern="_", replacement="\\\\_",  my.data[i, author.email[1]]),"") , "}\n", sep="")         
                                                             

										} else 
				{#more than one author, this part deals with the first author
						#presenting author first
						if(presenting.author[i]==1) {
							if(one.inst.true){ #only one institution 
										tmp<-paste("\\Presenting{", my.data[i, author.firstname[1] ], " ", my.data[i, author.lastname[1] ], "}",  
										#"\\index{", my.data[i, author.lastname[1] ], ", ", authors.initials,   "}",
										.fun.get.index.slo( my.data[i, author.lastname[1]] , authors.initials), sep="")} else 	
										#else, more than one institution
										tmp<-paste("\\Presenting{", my.data[i, author.firstname[1] ], " ", my.data[i, author.lastname[1] ], "}", "$^1$", 
										.fun.get.index.slo( my.data[i, author.lastname[1]] , authors.initials), sep="")} else{
						
						#else, presenting author not #1 
								
						if(one.inst.true){
										tmp<-paste(my.data[i, author.firstname[1] ], " ", my.data[i, author.lastname[1] ],  
												#"\\index{", my.data[i, author.lastname[1] ], ", ", authors.initials,   "}",
												.fun.get.index.slo( my.data[i, author.lastname[1]] , authors.initials), sep="")
									
									} else {
								#else. more than one institution
								tmp<-paste(my.data[i, author.firstname[1] ], " ", my.data[i, author.lastname[1] ], "$^1$", 
												#"\\index{", my.data[i, author.lastname[1] ], ", ", authors.initials,   "}",
												.fun.get.index.slo( my.data[i, author.lastname[1]] , authors.initials), sep="")}}# end else presenting author not 1
				} # end else more than one author
						
				#more than one author, this part deals with the other authors
				if(num.authors[i]>1)	
					for(j in 2:num.authors[i]){
						#getting author's initials
						#changed June12, using index.inst[j] instead of j to give the same number to the authors from the same institution
						TMP<-strsplit(as.character(my.data[,author.firstname[j]]), c(" "))
						number.of.names<- unlist(lapply(TMP, length))	
						
						#authors.initials<-toupper(unlist(lapply(TMP, function(x) strsplit(x, "")[[1]][1]))) 
						authors.initials<-toupper(strsplit(TMP[[i]][1], "")[[1]][1]) 
						if(number.of.names[i]>1) for(jj in 2:number.of.names[i])
								authors.initials<-paste(authors.initials, toupper(strsplit(TMP[[i]][jj], "")[[1]][1]), sep="") 
						
						#not last author
						if(j<num.authors[i]) {
						
									#only one institution
									if(one.inst.true){				
										if(presenting.author[i]==j) 
												tmp<-paste(tmp, ", \\Presenting{", paste(my.data[i, author.firstname[j] ], " ", my.data[i, author.lastname[j] ], "}",   
													#"\\index{", my.data[i, author.lastname[j] ], ", ", authors.initials,   "}", 	sep=""),
													.fun.get.index.slo( my.data[i, author.lastname[j]] , authors.initials), #"}", 
													sep=""))  else tmp<-paste(tmp, paste(my.data[i, author.firstname[j] ], " ", my.data[i, author.lastname[j] ], 
																	#"\\index{", my.data[i, author.lastname[j] ], ", ", authors.initials,   "}",
																	.fun.get.index.slo( my.data[i, author.lastname[j]] , authors.initials), 
																	sep=""), sep=", ")
													} else #more than one institution
									
									{if(presenting.author[i]==j) tmp<-paste(tmp, ", \\Presenting{", paste(my.data[i, author.firstname[j] ], " ", my.data[i, author.lastname[j] ], "}$^", index.inst[j], "$", 
																	#"\\index{", my.data[i, author.lastname[j] ], ", ", authors.initials,   "}", 	sep=""),
																	.fun.get.index.slo( my.data[i, author.lastname[j]] , authors.initials), #"}", 
																	sep=""), sep="")  else tmp<-paste(tmp, paste(my.data[i, author.firstname[j] ], " ", my.data[i, author.lastname[j] ], "$^", index.inst[j], "$", 
																							#"\\index{", my.data[i, author.lastname[j] ], ", ", authors.initials,   "}",
																							.fun.get.index.slo( my.data[i, author.lastname[j]] , authors.initials), 
																								sep=""), sep=", ") }#end else - more than one institution
									#end else: start if last author
									} else {
					
							#one institutione
							if(one.inst.true){
							if(presenting.author[i]==j) #tmp<-paste(tmp, " and \\Presenting{", paste(my.data[i, author.firstname[j] ], " ", my.data[i, author.lastname[j] ],  "}$^", index.inst[j], "$",
								tmp<-paste(tmp, " and \\Presenting{", paste(my.data[i, author.firstname[j] ], " ", my.data[i, author.lastname[j] ],  "}",
					#"\\index{", my.data[i, author.lastname[j] ], ", ", authors.initials,   "}", 
							.fun.get.index.slo( my.data[i, author.lastname[j]] , authors.initials), 
						sep=""), sep="")  else tmp<-paste(tmp, paste(my.data[i, author.firstname[j] ], " ", my.data[i, author.lastname[j] ], 
					  #"\\index{", my.data[i, author.lastname[j] ], ", ", authors.initials,   "}",
						.fun.get.index.slo( my.data[i, author.lastname[j]] , authors.initials), 
						sep=""), sep=" and ")} else # more than one institution
					
										
							
							{if(presenting.author[i]==j) tmp<-paste(tmp, "and \\Presenting{", paste(my.data[i, author.firstname[j] ], " ", my.data[i, author.lastname[j] ],  "}$^", index.inst[j], "$",
					#"\\index{", my.data[i, author.lastname[j] ], ", ", authors.initials,   "}", 
							.fun.get.index.slo( my.data[i, author.lastname[j]] , authors.initials), 
						sep=""), sep="")  else tmp<-paste(tmp, paste(my.data[i, author.firstname[j] ], " ", my.data[i, author.lastname[j] ],  "$^", index.inst[j], "$", 
					  #"\\index{", my.data[i, author.lastname[j] ], ", ", authors.initials,   "}",
						.fun.get.index.slo( my.data[i, author.lastname[j]] , authors.initials), 
						sep=""), sep=" and ")} # end more than one institution
					
					
					}#end else last author
					
					}#end for j
					

				################authors################
				autori[i]<-paste("{", tmp, "}", sep="")
				##################################
				
			#####################end names of authors ###################################

				############affiliations###############
				# example of affiliations {\Afilliation{$^1$National Institute of Biology, Slovenia}; \Email{andrej.blejec@nib.si;} \Afilliation{$^2$University of Ljubljana, Slovenia}; #\Email{lara.lusa@mf.uni-lj.si}}

				#one author
				if(num.authors[i]==1) {

				tmp<-paste("\\Afilliation{",  
					ifelse(my.data[i, author.institution[1] ]!="", paste(my.data[i, author.institution[1] ], ", ", sep=""), ""), 
					ifelse(my.data[i, author.city[1] ]!="", paste(my.data[i, author.city[1] ], ", ", sep=""), ""), 
					ifelse(my.data[i, author.country[1] ]!="", paste(my.data[i, author.country[1] ]), ""), 	
					"}\n", sep="") #\\Email{", 
					#ifelse(!is.na(my.data[i, author.email[1] ]),paste(my.data[i, author.email[1] ]),"") , 
					#"}
					#"}\n", sep="")  } else #more than 1 author
				} else #more than 1 author
				
				{#more than 1 author
				
					#all the authors come from the same institution
					if(one.inst.true){
					
						#changed June2012, no indexing of the authors just for the e-mails	
						tmp<-paste("\\Afilliation{", 	
								ifelse(my.data[i, author.institution[1] ]!="", paste(my.data[i, author.institution[1] ], ", ", sep=""), ""), 
								ifelse(my.data[i, author.city[1] ]!="", paste(my.data[i, author.city[1] ], ", ", sep=""), ""), 
								ifelse(my.data[i, author.country[1] ]!="", paste(my.data[i, author.country[1] ]), ""), 
								"}\n", sep="")
								
						#first author email		
						tmp.mails=paste("\\Email{", 
								ifelse(!is.na(my.data[i, author.email[1] ]),paste(my.data[i, author.email[1] ]),"") , 
								"}\n", sep="")  

						
							
						#e-mails of the other authors
						for(j in 2:num.authors[i]){
							tmp.mails<-paste(tmp.mails, "$\\Email{", 
								ifelse(!is.na(my.data[i, author.email[j] ]),paste(my.data[i, author.email[j] ]),"") , 
								"}\n", sep="")  
							#end for j} 
							} 
							} else {
							#else: there are different insitutions
							#start with data for the first author
							tmp<-paste("\\Afilliation{$^", 1, "$", 
								ifelse(my.data[i, author.institution[1] ]!="", paste(my.data[i, author.institution[1] ], ", ", sep=""), ""), 
								ifelse(my.data[i, author.city[1] ]!="", paste(my.data[i, author.city[1] ], ", ", sep=""), ""), 
								ifelse(my.data[i, author.country[1] ]!="", paste(my.data[i, author.country[1] ]), ""), 
								"}\n ", sep="")  


							#if(num.authors[i]>1)	
							#for(j in 2:num.authors[i]){
							#changed June2012, not listing all the institutions but just the unique insitutions
							
							# add data for the other authors, if there are any
							if(num.authors[i]>1){	
							for(j in unique(index.inst)[-1]){
								#index that determines the data of which author should be used for the other insitututions
								#example: index.inst=c(1,1,2), only 2 is used in this step, and the correct data can be retrieved using the data from author 3
								which.author.use=which(j==index.inst)[1] 
								
							#	if(index.inst[j] - old version
								tmp<-paste(tmp, "\\Afilliation{$^", j, "$", 
									ifelse(my.data[i, author.institution[which.author.use] ]!="", paste(my.data[i, author.institution[which.author.use] ], ", ", sep=""), ""), 
									ifelse(my.data[i, author.city[which.author.use] ]!="", paste(my.data[i, author.city[which.author.use] ], ", ", sep=""), ""), 
									ifelse(my.data[i, author.country[which.author.use] ]!="", paste(my.data[i, author.country[which.author.use] ]), ""), 
									"}\n", sep="")  #\\Email{", 
									#"}  \\Email{", 
									#ifelse(!is.na(my.data[i, author.email[j] ]),paste(my.data[i, author.email[j] ]),"") , 
									#"}\n", sep="")
								#June2012, removed the indication of the email at this level, put down.			
							}#end for j
						}# end else more than 1 affiliation
						}# end else more than 1 author for affiliation, more than an institution


				#changed June2012, all the e-mails are reported towgether, without the indexing
				#mail of the first author
				tmp.mails=paste("\\Email{",  
							ifelse(!is.na(my.data[i, author.email[1] ]),paste(my.data[i, author.email[1] ]), "") , 
							"}", sep="")
							
				for(j in 2:num.authors[i])
				#tmp=paste(tmp, "\\Email{" 
				#			ifelse(!is.na(my.data[i, author.email[j] ]),paste(my.data[i, author.email[j] ]),"") , 
				#			"}", sep=", ")
				tmp.mails=paste(tmp.mails, paste("\\Email{",  
							ifelse(!is.na(my.data[i, author.email[j] ]),paste(my.data[i, author.email[j] ]), "") , 
							"}", sep=""), sep=", ")
				
				
				} #end more than one author
				tmp=paste(tmp, tmp.mails, sep="\n")	

				#####################affiliations######################
				affiliazioni[i]<-paste("{", tmp, "}", sep="")
				#################################################


				####################abstracts############################
				abstract[i]<-paste("{",  my.data[i,pres.abstract],  "}", sep="")
				######################################################
				


				########selected topics, abstract ID
				#temp<-paste("{Topic1: ",  my.data[i,topic1], ", Topic2: ",  my.data[i,topic2], ". Abstract ID: ", my.data[i,id],  "}", sep="")
			 if(noNotes==F)	temp<-paste("{Topic1: ",  my.data[i,topic1], ", Topic2: ",  my.data[i,topic2], ". Abstract ID: ", my.data[i,id], ". Accepted: ", my.data[i, accept],   
			  ". Notes: ",  my.data[i,notes],  
			  ". Ref1: ", my.data[i, ref2],
			  ". Ref2: ", my.data[i, ref3],
			  ". Ref3: ", my.data[i, ref4],    "}", sep="") else temp<-paste("{Abstract ID: ", my.data[i,id], ". Topic1: ",  my.data[i,topic1], ", Topic2: ",  my.data[i,topic2],  ".}",  sep="")


				##################write the abstract files, one file for each abstract, the abstract ID is used to name the files, 1.tex, 2.tex, etc#######################
				zz<-file(paste(my.data[i,id], ".tex", sep=""), "w")
				cat("\\A", titolo[i], autori[i], affiliazioni[i], temp, abstract[i], sep="\n", file=zz)
				close(zz)
				##################end writing of files######################


				}#end for i
		#end if(style=="AS2012")
		} else{
		#style=="AS2011", generates the abstracts using the style of AS 2011 Conference
			for(i in 1:dim(my.data)[1]){

			if(verbose==TRUE) cat("ID=", i)

			##########title##################################
			titolo[i]<-paste("{", my.data[i,pres.title]     , "}", sep="")
			###########end title#################################


			########### authors names##############################
			#getting author's initials
			TMP<-strsplit(as.character(my.data[,author.firstname[1]]), c(" "))
			#number of different names in first name
			number.of.names<- unlist(lapply(TMP, length))
			#derive initials of first names, used in the index
			authors.initials<-toupper(strsplit(TMP[[i]][1], "")[[1]][1]) 
			if(number.of.names[i]>1) for(j in 2:number.of.names[i])
			authors.initials<-paste(authors.initials, toupper(strsplit(TMP[[i]][j], "")[[1]][1]), sep="") 

			#determine if all the authors come from just one instutution, true if it is the same for all authors, false otherwise
			#will be used to write the insitution just once if TRUE
			one.inst.true<- length(unique(unlist((my.data[i,author.institution[1:num.authors[i]]]))))==1 & length(unique(unlist((my.data[i,author.city[1:num.authors[i]]]))))==1
			
			
			#derive a string containing the  author names,  as they will appear in the abstract
			#added a different way to report 1 single author, no underlying and no number 1
			if(num.authors[i]==1) {tmp<-paste(my.data[i, author.firstname[1] ], " ", my.data[i, author.lastname[1] ], 
		  #"\\index{", my.data[i, author.lastname[1] ], ", ", authors.initials,   "}", 
		   .fun.get.index.slo( my.data[i, author.lastname[1]] , authors.initials), 
		  sep="")} else {
		if(presenting.author[i]==1) tmp<-paste("\\Presenting{", my.data[i, author.firstname[1] ], " ", my.data[i, author.lastname[1] ], "}", "$^1$", 
		#"\\index{", my.data[i, author.lastname[1] ], ", ", authors.initials,   "}",
			.fun.get.index.slo( my.data[i, author.lastname[1]] , authors.initials), 
		 sep="") else tmp<-paste(my.data[i, author.firstname[1] ], " ", my.data[i, author.lastname[1] ], "$^1$", 
		 #"\\index{", my.data[i, author.lastname[1] ], ", ", authors.initials,   "}",
			.fun.get.index.slo( my.data[i, author.lastname[1]] , authors.initials), 
		  sep="")


			if(num.authors[i]>1)	for(j in 2:num.authors[i]){


			#getting author's initials
			TMP<-strsplit(as.character(my.data[,author.firstname[j]]), c(" "))
			number.of.names<- unlist(lapply(TMP, length))	
			#authors.initials<-toupper(unlist(lapply(TMP, function(x) strsplit(x, "")[[1]][1]))) 
			authors.initials<-toupper(strsplit(TMP[[i]][1], "")[[1]][1]) 
			if(number.of.names[i]>1) for(jj in 2:number.of.names[i])
			authors.initials<-paste(authors.initials, toupper(strsplit(TMP[[i]][jj], "")[[1]][1]), sep="") 
			if(j<num.authors[i]) {
				if(presenting.author[i]==j) tmp<-paste(tmp, ", \\Presenting{", paste(my.data[i, author.firstname[j] ], " ", my.data[i, author.lastname[j] ], "}$^", j, "$", 
			#"\\index{", my.data[i, author.lastname[j] ], ", ", authors.initials,   "}", 	sep=""),
			   .fun.get.index.slo( my.data[i, author.lastname[j]] , authors.initials), #"}", 
			 sep=""))  else tmp<-paste(tmp, paste(my.data[i, author.firstname[j] ], " ", my.data[i, author.lastname[j] ], "$^", j, "$", 
			 #"\\index{", my.data[i, author.lastname[j] ], ", ", authors.initials,   "}",
				.fun.get.index.slo( my.data[i, author.lastname[j]] , authors.initials), 

				sep=""), sep=", ") 
				} else {if(presenting.author[i]==j) tmp<-paste(tmp, "and \\Presenting{", paste(my.data[i, author.firstname[j] ], " ", my.data[i, author.lastname[j] ],  "}$^", j, "$",
		#"\\index{", my.data[i, author.lastname[j] ], ", ", authors.initials,   "}", 
				.fun.get.index.slo( my.data[i, author.lastname[j]] , authors.initials), 
			sep=""), sep="")  else tmp<-paste(tmp, paste(my.data[i, author.firstname[j] ], " ", my.data[i, author.lastname[j] ],  "$^", j, "$", 
		  #"\\index{", my.data[i, author.lastname[j] ], ", ", authors.initials,   "}",
			.fun.get.index.slo( my.data[i, author.lastname[j]] , authors.initials), 
			sep=""), sep=" and ")
		}}
		}# end else num.authors more than 1

			################authors################
			autori[i]<-paste("{", tmp, "}", sep="")
			##################################
			
		#####################end names of authors ###################################

			############affiliations###############
			# example of affiliations {\Afilliation{$^1$National Institute of Biology, Slovenia}; \Email{andrej.blejec@nib.si;} \Afilliation{$^2$University of Ljubljana, Slovenia}; #\Email{lara.lusa@mf.uni-lj.si}}
			if(num.authors[i]==1) {

			tmp<-paste("\\Afilliation{",  
				ifelse(my.data[i, author.institution[1] ]!="", paste(my.data[i, author.institution[1] ], ", ", sep=""), ""), 
				ifelse(my.data[i, author.city[1] ]!="", paste(my.data[i, author.city[1] ], ", ", sep=""), ""), 
				ifelse(my.data[i, author.country[1] ]!="", paste(my.data[i, author.country[1] ]), ""), 	
				"}; \\Email{", 
				ifelse(!is.na(my.data[i, author.email[1] ]),paste(my.data[i, author.email[1] ]),"") , 
				"}\n", sep="")  } else 
			{
			if(one.inst.true){
				tmp<-paste("\\Afilliation{", 	
					ifelse(my.data[i, author.institution[1] ]!="", paste(my.data[i, author.institution[1] ], ", ", sep=""), ""), 
					ifelse(my.data[i, author.city[1] ]!="", paste(my.data[i, author.city[1] ], ", ", sep=""), ""), 
					ifelse(my.data[i, author.country[1] ]!="", paste(my.data[i, author.country[1] ]), ""), 
					"}\n
					$^1$\\Email{", 
					ifelse(!is.na(my.data[i, author.email[1] ]),paste(my.data[i, author.email[1] ]),"") , 
					"}\n", sep="")  

			#if(num.authors[i]>1)	
			for(j in 2:num.authors[i]){
				tmp<-paste(tmp, "$^", j, "$\\Email{", 
					ifelse(!is.na(my.data[i, author.email[j] ]),paste(my.data[i, author.email[j] ]),"") , 
					"}\n", sep="")  
				#end for j} 
				} } else {
				tmp<-paste("\\Afilliation{$^", 1, "$", 
					ifelse(my.data[i, author.institution[1] ]!="", paste(my.data[i, author.institution[1] ], ", ", sep=""), ""), 
					ifelse(my.data[i, author.city[1] ]!="", paste(my.data[i, author.city[1] ], ", ", sep=""), ""), 
					ifelse(my.data[i, author.country[1] ]!="", paste(my.data[i, author.country[1] ]), ""), 
					"}; \\Email{", 
					ifelse(!is.na(my.data[i, author.email[1] ]),paste(my.data[i, author.email[1] ]),"") , 
					"}\n", sep="")  


				#if(num.authors[i]>1)	
				for(j in 2:num.authors[i]){

					tmp<-paste(tmp, "\\Afilliation{$^", j, "$", 
						ifelse(my.data[i, author.institution[j] ]!="", paste(my.data[i, author.institution[j] ], ", ", sep=""), ""), 
						ifelse(my.data[i, author.city[j] ]!="", paste(my.data[i, author.city[j] ], ", ", sep=""), ""), 
						ifelse(my.data[i, author.country[j] ]!="", paste(my.data[i, author.country[j] ]), ""), 
						"}; \\Email{", 
						ifelse(!is.na(my.data[i, author.email[j] ]),paste(my.data[i, author.email[j] ]),"") , 
						"}\n", sep="")  
				}#end for j
			}# end else more than 1 affiliation

			}# end else more than 1 author for affiliation

			#####################affiliations######################
			affiliazioni[i]<-paste("{", tmp, "}", sep="")
			#################################################


			####################abstracts############################
			abstract[i]<-paste("{",  my.data[i,pres.abstract],  "}", sep="")
			######################################################
			


			########selected topics, abstract ID
			#temp<-paste("{Topic1: ",  my.data[i,topic1], ", Topic2: ",  my.data[i,topic2], ". Abstract ID: ", my.data[i,id],  "}", sep="")
		 if(noNotes==F)	temp<-paste("{Topic1: ",  my.data[i,topic1], ", Topic2: ",  my.data[i,topic2], ". Abstract ID: ", my.data[i,id], ". Accepted: ", my.data[i, accept],   
		  ". Notes: ",  my.data[i,notes],  
		  ". Ref1: ", my.data[i, ref2],
		  ". Ref2: ", my.data[i, ref3],
		  ". Ref3: ", my.data[i, ref4],    "}", sep="") else temp<-paste("{Abstract ID: ", my.data[i,id], ". Topic1: ",  my.data[i,topic1], ", Topic2: ",  my.data[i,topic2],  ".}",  sep="")


			##################write the abstract files, one file for each abstract, the abstract ID is used to name the files, 1.tex, 2.tex, etc#######################
			zz<-file(paste(my.data[i,id], ".tex", sep=""), "w")
			cat("\\A", titolo[i], autori[i], affiliazioni[i], temp, abstract[i], sep="\n", file=zz)
			close(zz)
			##################end writing of files######################


			}#end for i

			
			
			
			}#end else - part that uses the style of AS2011
			
		##############writes a file abstractList.tex 
		zz<-file(paste("abstractList.tex", sep=""), "w")
		for(i in my.data[,id])
			cat("\\input{", i, ".tex}\n", sep="", file=zz)	
		close(zz)

		
		##############writes a file abstractListAccepted.tex , with the list of the accepted abstracts
		zz<-file(paste("abstractListAccepted.tex", sep=""), "w")
		for(i in my.data[my.data[,accept]=="Yes",id])
			cat("\\input{", i, ".tex}\n", sep="", file=zz)	
		close(zz)



		################# writes the list of abstracts by topic ######################
		
		all.topics<-sort(unique(levels(my.data[,topic1]), levels(my.data[,topic2])))
		#removing the empty topic
		all.topics<-all.topics[all.topics!="---" | all.topics==""]

		zz<-file("abstractListByTopic.tex", "w")

		#browsing through topics
		for( ii in 1:length(all.topics)){
				which.abstracts=unique(which(my.data[,topic1]==all.topics[ii] | my.data[,topic1]==all.topics[ii]))
				cat("{\\bf \\Large " ,  as.character(all.topics[ii]),    "}\\\\\\\\", sep=" ", file=zz)
					for(i in which.abstracts)
					cat(titolo[i], "\\\\", autori[i], "\\\\", affiliazioni[i], paste(" ID=", my.data[i,id], "; Day=", my.data[i, notesDay], "Payment=", my.data[i, notesPayment],  "\\\\ Topics={\\small ", as.character(my.data[i,topic1]), as.character(my.data[i,topic2]), "} \\\\\\\\", sep=" "),   sep=" ", file=zz)
	
				cat("\\\\ \\clearpage", file=zz)

		}# end for ii

	close(zz)
	

################# writes the list of abstracts by topic, for the accepted abstracts only ######################
		
		#same as for abstractListByTopicAccepted.tex, but considers only the accepted abstracts
		
		my.data.accepted=my.data[my.data[,accept]=="Yes",]
		all.topics=sort(unique(levels(my.data.accepted[,topic1]), levels(my.data.accepted[,topic2])))
		#removing the empty topic
		all.topics=all.topics[all.topics!="---" | all.topics==""]

		zz<-file("abstractListByTopicAccepted.tex", "w")

		#browsing through topics
		for( ii in 1:length(all.topics)){
				which.abstracts=unique(which(my.data.accepted[,topic1]==all.topics[ii] | my.data.accepted[,topic1]==all.topics[ii]))
				cat("{\\bf \\Large " ,  as.character(all.topics[ii]),    "}\\\\\\\\", sep=" ", file=zz)
					for(i in which.abstracts)
					cat(titolo[my.data[,accept]=="Yes"][i], "\\\\", autori[my.data[,accept]=="Yes"][i], "\\\\", affiliazioni[my.data[,accept]=="Yes"][i], paste(" ID=", my.data.accepted[i,id], "; Day=", my.data.accepted[i, notesDay], "Payment=", my.data.accepted[i, notesPayment],  "\\\\ Topics={\\small ", as.character(my.data.accepted[i,topic1]), as.character(my.data.accepted[i,topic2]), "} \\\\\\\\", sep=" "),   sep=" ", file=zz)
	
				cat("\\\\ \\clearpage", file=zz)

		}# end for ii

	close(zz)
		
		

		
		#restore the intial working directory in R
		setwd(initial.wd)
		
    return(list(My.data=my.data, PA=presenting.author))
  	}##################end of generate.abstracts############################



###############generate calls to the abstract files







############## generate a separate file for each decision.


#' Function that writes in a directory a separate file containing the submissions for each decision 
#' @param my.data name of the R data.frame object containing the submissions, can be retrieved from the output obtained from the generate.abstracts function (first output)
#' @param dirData directory where the files will be stored, it must exist
#' @param accept column in the data.frame that contains the decision
#' @param author.lastname columns that contain the last name of the authors in the contribution data base (numbers must be gives, i.e. if only two authors are allowed and the last names are stored in the 5th and 10th column of the contribution database, author.lastname=c(5,10)) 
#' @param author.firstname columns that contain thefirst name of the authors
#' @param author.email columns that contain the e-mails of the authors
#' @param presenting.author index that indicates the order of the presenting author for each submission, can be retrieved from the output obtained from the generate.abstracts function (second output) 
#' @param file.names prefix of the files names that will be generated, default is Submissions.The suffix will be given by the values included in the accept column 
#' @export
#' @return The names of the files being generated. The functions writes tab-delimted text files in the dirData directory. Adds additional columns indicating the name and last name of the presenting author, and his/her e-mail.
#' @examples 
#' set.seed(1)
 

generate.files.byDecision=function(my.data,   dirData,  accept=17, 
                                   author.lastname=seq(23, by=6, length.out=7), 
                                   author.firstname=seq(24, by=6, length.out=7), 
                                   author.email=seq(25, by=6, length.out=7), 
                                   presenting.author,
                                   file.names="Submissions"){
  
  #possible entries in the accept column
  my.levels.accept=levels(as.factor(my.data[,accept]))
  if(length(my.levels.accept)==0) stop("There are no data in the column that you indicated as including the acceptance decisions")
  my.names.files=paste0(file.names, my.levels.accept,  ".txt")
  
  
  
  Presenting.Author.email=sub("\\\\_", "_", unlist(lapply(1:length(presenting.author), function(i) my.data[i,author.email][presenting.author[i]])))
  Presenting.Author.firstname=lapply(1:length(presenting.author), function(i) my.data[i,author.firstname][presenting.author[i]])
  Presenting.Author.lastname=lapply(1:length(presenting.author), function(i) my.data[i,author.lastname][presenting.author[i]])
  
  Presenting.Author.lastname= unlist(lapply(1:length(Presenting.Author.lastname), function(x) Presenting.Author.lastname[[x]][[1]]))
  Presenting.Author.firstname= unlist(lapply(1:length(Presenting.Author.firstname), function(x) Presenting.Author.firstname[[x]][[1]]))
  
  
  my.data=data.frame(my.data, Presenting.Author.lastname,  Presenting.Author.firstname,  Presenting.Author.email)
  
  
  setwd(dirData)
  
  for(i in 1:length(my.levels.accept)){
    
    my.data.tmp=my.data[my.data[,accept]==my.levels.accept[i],]
    
    write.table(my.data.tmp, file=my.names.files[i], sep="\t", row.names = FALSE)
    
  }
  
    
  return(file.names=file.path(dirData, file.names))
  
  
  
}#end generate.files.byDecision





##############################








#split  names with more than 25 characters
#here we assume the the maximum length is 50 characters, so that splitting in two rows at most is ok


#' Function that generates the program overview file
#' @param my.program.file: name of the tab-delimited file where the program is saved
#' @param dirAbstracts: directory where the abstracts will be stored, it must exist
#' @param abstract.id: colums where the abstracts to be included in each session are reported
#' @export
#' @examples 
#' set.seed(1)

generate.programOverview<-function(my.program.file, dirAbstracts, abstract.id=c(10:14)){
#my.program.file: name of the tab-delimited file where the program is saved
#dirAbstracts: directory where the abstracts will be stored, it must exist
#abstract.id: colums where the abstracts to be included in each session are reported

init.wd=getwd()

#read the program
my.program<-read.delim(my.program.file, sep="\t")
my.program<-my.program[!is.na(my.program[,1]) & my.program[,1]!="",]

#number of rows included in the program
num.rows<-dim(my.program)[1]

setwd(dirAbstracts)

#obtain ordered program
my.order<-order(my.program$Day*100+my.program$TimeBegin)
my.program<-my.program[my.order,]

#number of different days
number.days<-unique(my.program$Day)

#number of rooms 
room.names<-unique(my.program$Room[my.program$Room!=""])
number.rooms<-length(room.names)

#times
my.time<-my.program$Day*100+my.program$TimeBegin


#number of abstracts per session
number.abstracts<-as.numeric(apply(my.program[,abstract.id], 1, function(x) sum(!is.na(x))))


#which session names have to be split because they are too long, max 25 characters per line, therefore max lenght is 50 characters
which.split<-ifelse(nchar(as.character(my.program$Name))>=25, 1, 0)

#added an extra element at the end, that will be used for session at times where not all the rooms have a session
where.split<-rep(NA, num.rows+1)
names.split<-vector("list", num.rows+1)

names.split[[num.rows+1]]<-""
where.split[num.rows+1]<-2

#same thing for the names of the session
my.program.name<-c(as.character(my.program$Name), "")


#point where names must be split
for(i in c(1:num.rows)[which.split==1]){
	names.split[[i]]<-tmp<-strsplit(as.character(my.program$Name[i]), split=" ")
	where.split[i]<-which(cumsum(unlist(lapply(tmp, nchar)))>=25)[1]
	#if the last word is "too" long
	if(cumsum(unlist(lapply(tmp, nchar)))[length(tmp[[1]])]<=25) where.split[i]<-length(tmp[[1]])
}

#modified sept1/2010: to fix a bug that precluded the printing in the program overview of session where one had to be split and the other hadn't 
for(i in c(1:num.rows)[which.split==0]){
	names.split[[i]]<-tmp<-as.character(my.program$Name[i])
	#where.split[i]<-which(cumsum(unlist(lapply(tmp, nchar)))>=25)[1]
	#if the last word is "too" long
	#if(cumsum(unlist(lapply(tmp, nchar)))[length(tmp[[1]])]<=25) where.split[i]<-length(tmp[[1]])
}






#deriving where.split for the names that do not need to be split
for(i in c(1:num.rows)[which.split==0]){
	tmp<-strsplit(as.character(my.program$Name[i]), split=" ")
	where.split[i]<-length(tmp)[[1]]+1
}

#added the last element as for where.splot, names.split, etc
which.split<-c(which.split, 0)

#paste(unlist(tmp)[1:(where.split[i]-1)], concatenate=" ")


#indicator for avoiding lines that were already included in the table - same time, different room
my.done<-rep(FALSE, num.rows)

zz<-file("programOverview.tex", "w")

cat("\\noindent\\\\

\\thispagestyle{empty}
 \\begin{center}
  \\Large
   % \\textbf{Program} \\\\ [0.5cm]
   \\begin{flushright}
   \\vspace{17cm} {\\Huge \\em{ \\textbf{PROGRAM}}} \\\\ [0.5cm]
   \\end{flushright}
   \\normalsize
 \\end{center}
%\\noindent  \\hrulefill \\\\[0.5cm]
\\small
\\clearpage


\\pagestyle{fancy}
\\renewcommand{\\Date}{}
\\addtocontents{toc}{\\hfill\\textbf{\\Date}\\\\}

%% ------------------------------------------- Session start
\\renewcommand{\\Session}{}
\\SetHeader{}{\\textbf{Program Overview}}
%%--------------------------------------------


\\vspace*{-1.0cm}
\\begin{center} %end of the first cat, header of the file and beginning of the table
\\begin{tabular}{|l|| l |", file=zz) 
cat(rep("c", number.rooms, sep=" "), "|}\\hline", file=zz)
cat("&&", paste(room.names, collapse="&"), "\\\\\\hline\\hline\n", file=zz) #end cat

#counter within the day
day.within<-0
#indicator for end of the day reached with the previous wirtten record
day.changed<-FALSE

for(i in 1:num.rows){
cat(i, "\n")

	#checking if this record was already included
	if(my.done[i]==FALSE){
	
	#check if there is a change of day
	if(i>1) { if(my.program$DayTable[i]!=my.program$DayTable[i-1]) day.within<-0
    #check if there was a a change in the day in the next record, and write the line uder the previous record, type of line depends on the change of the day or not
		if(day.changed==T) my.line<-"\\\\\\hline\\hline" else my.line<-paste("\\\\\\cline{2-", number.rooms+2, "}")
		cat(my.line, "\n",	file=zz)
		} 
	
	#calculate time of the session

	if(my.program$TimeBegin[i]>10) time.begin<-strsplit(as.character(my.program$TimeBegin[i]), "")[[1]][1:5] else time.begin<-strsplit(as.character(my.program$TimeBegin[i]), "")[[1]][1:4]
if(!is.na(my.program$TimeEnd[i])) {if(my.program$TimeEnd[i]>10) time.end<-strsplit(as.character(my.program$TimeEnd[i]), "")[[1]][1:5] else 	time.end<-strsplit(as.character(my.program$TimeEnd[i]), "")[[1]][1:4]} else time.end<-NA



	#writing the date at the first entry for the day
		if(day.within==0) tmp.day<-as.character(my.program$DayTable[i]) else tmp.day<-""
		day.within<-day.within+1

		
		
		#are there any other sessions at the same time and which
		which.same.time<-which(my.time[i]==my.time)

		#common session, without any room specified, like breaks, llunch, etc, it is centered in the table
		if(my.program$Room[i]=="") {tmp.session<-paste("\\multicolumn{", number.rooms, "}{c|}{\\cellcolor[gray]{0.9}", my.program$Name[i], "}")
     		#tmp.time<-paste(paste(time.begin, collapse=""), " -- ", paste(time.end, collapse=""))
			tmp.time<-paste(paste(time.begin, collapse=""), ifelse(!is.na(time.end[1]), paste(" --", paste(time.end, collapse="")), ""), sep="") 
		
		
			cat(paste(tmp.day, tmp.time, tmp.session, sep="&"), file=zz)
			if(i<num.rows) day.changed<-ifelse(my.program$Day[i]!=my.program$Day[i+1], TRUE, FALSE) 
			#cat("\\\\\\cline{2-", number.rooms+2, "} \n", file=zz)

		#
		##### try Thr } else {if(length(which.same.time)>1)    {
		} else  {# not a session that has no room definition
		###} else {
			which.room<-vector("list", number.rooms) 
			which.index<-numeric(number.rooms)
			for(ii in 1:number.rooms){
				tmp<-which(my.program$Room==room.names[ii])	
				#INDEX FOR ROOM I AT THIS TIME
				iii<-tmp[is.element(tmp, which.same.time)]
				my.done[iii]<-TRUE
				#indexes to use for this time, ordered, if not all the rooms have a session a reference to an empty line is added
				which.index[ii]<-ifelse(length(iii)==1, iii, num.rows+1)
				#indicator of the latest used record - records should be ordered?
				my.max<-max(which.index[!is.element(which.index, num.rows+1)])

				if(i<num.rows) day.changed<-ifelse(my.program$Day[i]!=my.program$Day[my.max+1], TRUE, FALSE)
			}#end for ii
			
			#if there is the need to split the names of the sessions		
			if(any(which.split[which.index]==1)){
				#first part of name of the session - split beacuse they are too long		
				tmp.session<-paste(paste(unlist(lapply(which.index, function(j) paste(unlist(names.split[[j]])[1:(where.split[j]-1)], collapse=" "))), collapse="&"), "\\\\")
				#tmp.session.2<-paste(paste(unlist(lapply(which.index, function(j) paste(unlist(names.split[[j]])[-c(1:(where.split[j]-1))], collapse=" "))), collapse="&"), "\\\\\\cline{2-", number.rooms+2, "}", sep="")
				tmp.session.2<-paste(paste(unlist(lapply(which.index, function(j) paste(unlist(names.split[[j]])[-c(1:(where.split[j]-1))], collapse=" "))), collapse="&"),  sep="")
				tmp.time<-paste("\\multirow{2}{*}{", paste(time.begin, collapse=""), 
				ifelse(!is.na(time.end[1]), paste(" --", paste(time.end, collapse=""), "}"), "}"), sep="")
		

		
				cat(paste(tmp.day, tmp.time, tmp.session, sep="&"), file=zz)
				cat("\n", file=zz)

				cat(paste("", "", tmp.session.2, sep="&"), file=zz)
				cat("\n", file=zz)
			
			#no need to split the name of any of the sessions
#			} else	{tmp.session<-paste(paste(my.program$Name[which.index], collapse="&"), "\\\\", sep="")
			#} else	{tmp.session<-paste(paste(my.program.name[which.index], collapse="&"), "\\\\", sep="")
			} else	{tmp.session<-paste(paste(my.program.name[which.index], collapse="&"), sep="")
			#tmp.time<-paste(paste(time.begin, collapse=""), " -- ", paste(time.end, collapse=""))
			tmp.time<-paste(paste(time.begin, collapse=""), ifelse(!is.na(time.end[1]), paste(" --", paste(time.end, collapse="")), ""), sep="") 

			cat(paste(tmp.day, tmp.time, tmp.session, sep="&"), file=zz)
			my.max<-max(which.index[!is.element(which.index, num.rows+1)])

			if(i<num.rows) day.changed<-ifelse(my.program$Day[i]!=my.program$Day[my.max+1], TRUE, FALSE)
			
			}#end for ii
				
			
     		#cat(my.line, file=zz)

			cat("\n", file=zz)

			} # end else {tmp.session...}
			
			#}#end for ii	
		#end else which.same.time>1
		#} else {
		
		
			
		#}#end else which.same.time 1
		
		
		#end else which.same.time

		}# end if my.done


	}#end for i


cat("\\\\\\hline", "\n" , "\\end{tabular} \\end{center}
\\clearpage", file=zz)

close(zz)

#restoring initial working directory
setwd(init.wd)

}#########################end generate program overview#############################


#' Function that generates the program of the conference
#' @param my.program.file: name of the tab-delimited file where the program is saved
#' @param my.filename: name of the file with the abstract database
#' @param dirAbstracts: directory where the abstracts will be stored, it must exist
#' @param abstract.id: colums where the abstracts to be included in each session are reported
#' @param author.lastname: columns that contain the last name of the authors
#' @param author.first: columns that contain the first name of the authors
#' @param accept: column that contains the acceptance decision, must be " Yes" for the acceptance of the abstract
#' @param pres.title: column that contains the title of the presentation
#' @param id: abstract id column in the database of abstracts
#' @return empty return; TeX files are generated and stored dirAbstracts directory
#' @export
#' @examples 
#' set.seed(1)

generate.program<-function(my.program.file, my.filename, dirAbstracts, abstract.id=c(10:14), author.lastname=seq(23, by=6, length.out=7),  author.firstname=seq(24, by=6, length.out=7),  accept=17, pres.title=18, id=75, outfile.name="program.tex")
{
#my.program.file: name of the tab-delimited file where the program is saved
#my.filename: name of the file with the abstract database
#dirAbstracts: directory where the abstracts will be stored, it must exist
#abstract.id: colums where the abstracts to be included in each session are reported
#author.lastname: columns that contain the last name of the authors
#author.first: columns that contain the first name of the authors
#accept: column that contains the acceptance decision, must be " Yes" for the acceptance of the abstract
#pres.title: column that contains the title of the presentation
#id: abstract id column in the database of abstracts

#save initial working directory
init.wd=getwd()

#read the program
my.program<-read.delim(my.program.file, sep="\t")
my.program<-my.program[!is.na(my.program[,1]) & my.program[,1]!="",]

#number of rows included in the program
num.rows<-dim(my.program)[1]


#obtain ordered program
my.order<-order(my.program$Day*100+my.program$TimeBegin)
my.program<-my.program[my.order,]

#number of different days
number.days<-unique(my.program$Day)


#times
my.time<-my.program$Day*100+my.program$TimeBegin



num.rows<-dim(my.program)[1]

#read data from abstracts
my.data<-read.delim(my.filename, sep="\t")
#remove the empty records
#calculate the number of authors for each abstract
num.authors<-apply(my.data[,author.lastname], 1, function(x) sum(!is.na(x) & x!=""))
my.data<-my.data[num.authors!=0,]
#remove duplicated records and rejected papers, accepted paper must be indicated with "Yes" in the accept column
my.data<-my.data[my.data[,accept]=="Yes",]
#recalculate the number of authors
num.authors<-apply(my.data[,author.lastname], 1, function(x) sum(!is.na(x) & x!=""))

#number of abstracts per session
number.abstracts<-as.numeric(apply(my.program[,abstract.id], 1, function(x) sum(!is.na(x))))

setwd(dirAbstracts)


#zz<-file("program.tex", "w")
zz<-file(outfile.name, "w")

cat("\\clearpage \n \\pagestyle{fancy}
		% ------------------------------------------------- Next day -----
		\\renewcommand{\\Date}{}
		\\addtocontents{toc}{\\hfill\\textbf{\\Date}\\\\}
		%% ------------------------------------------- Session start
		\\renewcommand{\\Session}{}
		\\SetHeader{\\textbf{", as.character(my.program$DayLong[1]), 
		"}}{}", "\n", file=zz, sep="")


		
		

for(i in 1:num.rows){
#problems with too few decimal numbers.... trick to avoid 10.3 instead of 10.30 (input: 10.301)
cat(i, "\n")

#additional problem with hours starting begore 10, less figures!
if(my.program$TimeBegin[i]>10) time.begin<-strsplit(as.character(my.program$TimeBegin[i]), "")[[1]][1:5] else time.begin<-strsplit(as.character(my.program$TimeBegin[i]), "")[[1]][1:4]
if(!is.na(my.program$TimeEnd[i])) {if(my.program$TimeEnd[i]>10) time.end<-strsplit(as.character(my.program$TimeEnd[i]), "")[[1]][1:5] else 	time.end<-strsplit(as.character(my.program$TimeEnd[i]), "")[[1]][1:4]} else time.end<-NA
#write time
	cat("\\PrSectionHeader{", time.begin, sep="", file=zz)
	if(!is.na(time.end[1])) cat("--", time.end, "}", sep="", file=zz)  else cat("}", file=zz, sep="")

#wirte session name
	cat("{", as.character(my.program$Name[i]), "}", file=zz, sep="")
	
#wirte Hall
	if(my.program$Room[i]!="") cat("{(", as.character(my.program$Room[i]), ")}", file=zz, sep="") else cat("{}", file=zz, sep="")

	#write chair
if(my.program$Chair[i]!="") cat("{Chair: ", as.character(my.program$Chair[i]), "}", file=zz, sep="") else cat("{}", file=zz, sep="")

cat("\n", file=zz)


###############abstrac titles and authors###############

if(number.abstracts[i]>0){
	cat("\\begin{enumerate}\n", file=zz)
	for(ii in 1:number.abstracts[i]){

	cat(ii, " ")
	
		my.index<- which(my.data[,id]==my.program[i,abstract.id[ii]])

		tmp<-paste(my.data[my.index, author.firstname[1] ], " ", my.data[my.index, author.lastname[1] ], sep="")
		if(num.authors[my.index]>1)	for(j in 2:num.authors[my.index]){
			if(j<num.authors[my.index]) 	tmp<-paste(tmp, paste(my.data[my.index, author.firstname[j] ], " ", my.data[my.index, 		author.lastname[j] ], sep=""), sep=", ") else tmp<-paste(tmp, paste(my.data[my.index, author.firstname[j] ], " ", my.data[my.index, author.lastname[j] ], sep=""), sep=" and ") }

		autori<-tmp

#	cat("\\PrTalk{",  as.character(my.data[my.index, pres.title]), "} {", autori,  "}", sep="", file=zz)
#modified sept2010, the names of the authors in the program start at a new line
	cat("\\PrTalk{",  as.character(my.data[my.index, pres.title]), "} \\newline {", autori,  "}", sep="", file=zz)
	}#end for ii 
cat("\\end{enumerate}\n", file=zz)
} #end if number.abstracts>0




#check if the Day changes
if(i!=num.rows){
	if(my.program$Day[i+1]!=my.program$Day[i]){
		cat("\\clearpage \n \\pagestyle{fancy}
		% ------------------------------------------------- Next day -----
		\\renewcommand{\\Date}{}
		\\addtocontents{toc}{\\hfill\\textbf{\\Date}\\\\}
		%% ------------------------------------------- Session start
		\\renewcommand{\\Session}{}
		\\SetHeader{\\textbf{", as.character(my.program$DayLong[i+1]), 
		"}}{}", "\n", file=zz, sep="")
		}
}

}#end for i

close(zz)



#######################generate abstract list######################

###########Generate abstract lists######################


#sessionw with at least 1 abstract
session.id<- which(number.abstracts>0)

zz<-file("abstracts.tex", "w")


cat("\\noindent\\\\

\\thispagestyle{empty}
 \\begin{center}
  \\Large
   % \\textbf{Program} \\\\ [0.5cm]
   \\begin{flushright}
   \\vspace{17cm} {\\Huge \\em{ \\textbf{ABSTRACTS}}} \\\\ [0.5cm]
   \\end{flushright}
   \\normalsize
 \\end{center}
%\\noindent  \\hrulefill \\\\[0.5cm]
\\small
\\clearpage


\\pagestyle{fancy}
\\renewcommand{\\Date}{}
\\addtocontents{toc}{\\hfill\\textbf{\\Date}\\\\}

%% ------------------------------------------- Session start
\\renewcommand{\\Session}{}
\\SetHeader{}{\\textbf{Program Overview}}
%%--------------------------------------------









 %\\begin{center}
 %  \\Large
 %   \\textbf{Abstracts} \\\\ [0.5cm]
 %  \\normalsize
%  \\begin{flushright}
%   \\vspace{17cm} {\\Huge \\em{ \\textbf{ABSTRACTS}}} \\\\ [0.5cm]
%   \\end{flushright}
%   \\normalsize
 
 
% \\end{center}
%\\noindent\\  %\\hrulefill \\\\
\\small
\\clearpage 

\\pagestyle{fancy}

\\renewcommand{\\Date}{", 
as.character(my.program$DayShort[session.id[1]]), 
"}
\\addtocontents{toc}{\\hfill\\textbf{\\Date}\\\\}

\\renewcommand{\\Session}{", as.character(my.program$Name[session.id[1]]), "}
\\Section{\\Session}
\\SetHeader{\\Date}{\\Session}", file=zz, sep="")

 


for(i in 1:length(session.id)){
#problems with too few decimal numbers.... trick to avoid 10.3 instead of 10.30 (input: 10.301)
cat(i, "\n")

for(j in 1:number.abstracts[session.id[i]]){
cat("\\input{", my.program[session.id[i],abstract.id[j]], ".tex}\n", file=zz, sep="")
}#end for j
cat("\\clearpage\n", file=zz)

#not at the end
if(i!=length(session.id)){
cat("\\renewcommand{\\Session}{", as.character(my.program$Name[session.id[i+1]]), "}
\\Section{\\Session}
\\SetHeader{\\Date}{\\Session}", file=zz, sep="")

if(my.program$Day[session.id[i]]!=my.program$Day[session.id[i+1]]) 
cat("\\renewcommand{\\Date}{", 
as.character(my.program$DayShort[session.id[i+1]]), 
"}
\\addtocontents{toc}{\\hfill\\textbf{\\Date}\\\\}", file=zz, sep="") 

}#end if i!=session.id

}#end for i

close(zz)

#restore initial working directory
setwd(init.wd)

###########End Generate abstract lists######################
}###################end generate program###########################


#' Function that generates the duties of the participants
#' @param my.filename: tab delimited database that contains the abstract submission
#' @param my.filename.program: : tab delimited database that contains the program of the conference
#' @param dirAbstracts: directory where the abstracts will be stored, it must exist
#' @param author.lastname: columns that contain the last name of the authors
#' @param author.firstname: columns that contain thefirst name of the authors
#' @param author.presenting:columns that contain an indicator T/F indicating if the author will present the work
#' @param accept: column that contains the acceptance decision, must be " Yes" for the acceptance of the abstract
#' @param id: column that contains the abstract ID
#' @param abstract.id: columns in the program file that contain the abstract IDs to be included in each session
#' @return empty return; TeX files are generated and stored dirAbstracts directory
#' @export
#' @examples 
#' set.seed(1)


###my.filename<-"Abstracts for Applied Statistics 2010 1106"
#############function that generates the duties of the single participants
generate.duties<-function(my.filename, my.program.file, dirAbstracts,
					author.lastname=seq(23, by=6, length.out=7),
					author.firstname=seq(24, by=6, length.out=7),
					#author.institution=seq(26, by=6, length.out=7),
					#author.city=seq(27, by=6, length.out=7),
					#author.country=seq(3, by=1, length.out=7)+1,
					#author.email=seq(25, by=6, length.out=7),
					author.presenting=seq(22, by=6, length.out=7),
					#pres.title=18,
					#pres.abstract=21,
					accept=17,
					#topic1=19,
					#topic2=3,
					id=2, 
					abstract.id=10:14){


#my.filename: tab delimited database that contains the abstract submission
#my.filename.program: : tab delimited database that contains the program of the conference
#dirAbstracts: directory where the abstracts will be stored, it must exist
#author.lastname: columns that contain the last name of the authors
#author.firstname: columns that contain thefirst name of the authors
#author.presenting:columns that contain an indicator T/F indicating if the author will present the work
#accept: column that contains the acceptance decision, must be " Yes" for the acceptance of the abstract
#id: column that contains the abstract ID
#abstract.id: columns in the program file that contain the abstract IDs to be included in each session

#read data
	my.data<-read.delim(my.filename, sep="\t")
 my.program<-read.delim(my.program.file, sep="\t")
#setwd("abstracts09try")
#setwd(dirAbstracts)

#obtain the final database

#calculate the number of authors for each abstract
num.authors<-apply(my.data[,author.lastname], 1, function(x) sum(!is.na(x) & x!=""))
#remove the empty records
my.data<-my.data[num.authors!=0,]
#remove duplicated records and rejected papers, accepted paper must be indicated with "Yes" in the accept column
my.data<-my.data[my.data[,accept]=="Yes",]

#calculate the number of authors
#num.authors<-apply(my.data[,author.lastname], 1, function(x) sum(!is.na(x) & x!=""))
#calculate which is the presenting author, select just one if more than one was indicated as presenting
presenting.author<-as.numeric(unlist(apply(my.data[,author.presenting], 1, function(x) which(x=="Yes")[1])))
#the first author is the presenting author if none was indicated as presenting
presenting.author[is.na(presenting.author)]<-1
#my.results<-vector("list" )
my.results<-matrix(NA, ncol=9)
k<-0

#vector containing the row number of the chairs that were assigned
Which.chair<-NULL

for(i in 1:dim(my.data)[1]){
  k<-k+1

   #get the name
  {tmp.name<-c(as.character(my.data[i, author.firstname[presenting.author[i]] ]), as.character(my.data[i, author.lastname[presenting.author[i]] ]))}


for(ii in 1:dim(my.program)[1]){
  if(any(!is.na(my.program[ii,abstract.id])) & is.element(my.data[i, id], my.program[ii,abstract.id])) {
  which.presentation<-which(is.element(my.program[ii,abstract.id], my.data[i, id]))
  
  tmp.timehall<-c(as.character(my.program$DayTable[ii]), as.character(my.program$Room[ii]), as.character(my.program$TimeBegin[ii]), as.character(my.program$TimeEnd[ii]), as.character(my.program$Name[ii]), "Presenter", which.presentation)
  #break}
    my.results<-rbind(my.results, c(tmp.name, tmp.timehall))
 
  }

  
    } #end ii
 
 #checking if the presenter is also a chair
    which.chair<-which(is.element(as.character(my.program$Chair), paste(tmp.name[1], tmp.name[2])) )
    Which.chair<-c(Which.chair, which.chair)
 
if(!is.null(which.chair)) {
for(kk in which.chair){
  k<-k+1
   tmp.timehall<-c(as.character(my.program$DayTable[kk]), as.character(my.program$Room[kk]), as.character(my.program$TimeBegin[kk]), as.character(my.program$TimeEnd[kk]), as.character(my.program$Name[kk]), "Chair", 0)
  #my.results[[k]]<-c(tmp.name, tmp.timehall)
  my.results<-rbind(my.results, c(tmp.name, tmp.timehall))
  }
}
   }#end for i

 
 #obtaining the data for the chair that do not present 
my.program.reduced<-my.program[-Which.chair,][which(my.program[-Which.chair,]$Chair!=""),]
for(i in 1:dim(my.program.reduced)[1]){
  tmp.name<-unlist(strsplit(as.character(my.program.reduced[i,]$Chair), " "))[1:2]

        tmp.timehall<-c(as.character(my.program.reduced$DayTable[i]), as.character(my.program.reduced$Room[i]), as.character(my.program.reduced$TimeBegin[i]), as.character(my.program.reduced$TimeEnd[i]), as.character(my.program.reduced$Name[i]), "Chair", 0)
    my.results<-rbind(my.results, c(tmp.name, tmp.timehall))

}



  

	########selected topics, abstract ID
	#temp<-paste("{Topic1: ",  my.data[i,topic1], ", Topic2: ",  my.data[i,topic2], ". Abstract ID: ", my.data[i,id],  "}", sep="")

my.results<-my.results[-1,]	
my.results<-my.results[order(my.results[,2]),]

dimnames(my.results)[[2]]<-c("FirstName", "LastName", "Day", "Room", "TimeBegin", "TimeEnd", "Session", "Role", "Order")


return(my.results)	

	}##################end of generate.duties############################


	
	
	
	
	
	
	
	
	
	
	
	
	
###### new function, added Sept11, 	


#' Function that generates a separate TeX file for each of the abstracts
#' @param my.filename: tab delimited database that contains the abstract submission (contribution data base), full path must be given or it must be stored in the working directory of R
#' @param dirAbstracts: directory where the abstracts will be stored, it must exist
#' @param author.lastname: columns that contain the last name of the authors in the contribution data base (numbers must be gives, i.e. if only two authors are allowed and the last names are stored in the 5th and 10th column of the contribution database, author.lastname=c(5,10)) 
#' @param author.firstname: columns that contain thefirst name of the authors
#' @param author.institution: columns that contain the institution of the authors
#' @param author.city: columns that contain the city of the authors
#' @param author.country: columns that contain the country of the authors
#' @param author.email: columns that contain the e-mails of the authors
#' @param author.presenting: columns that contain an indicator T/F indicating if the author will present the work
#' @param pres.title: column containing the title of the presentation
#' @param pres.abstract: column containing the abstract text
#' @param accept: column that contains the acceptance decision, must be (exaclty) "Yes" for the accepted abstracts
#' @param topic1: column that contains the first selected topic
#' @param topic2: column that contains the second selected topic
#' @param id: column that contains the abstract ID
#' @param notes: column that contains the notes added by the reviewers
#' @param ref2: opinion of reviewer2
#' @param ref3: opinion of reviewer3
#' @param ref4: opinion of reviewer4
#' @param duplicated: logical, indicating if the abstract was submitted more than once
#' @param accept.all: if set to TRUE, all the abstracts included in the database are "accepted", useful to generate the first draft of the abstract book
#' @param notes1: column containing annotations (can be used for notes about the presentation day)
#' @param notes2: column containing annotations (can be used for notes about the payments/registration)
#' @return empty return; TeX files are generated and stored dirAbstracts directory
#' @export
#' @examples 
#' set.seed(1) 

generate.submissionOverview.byTopic<-function(my.filename,  dirAbstracts, 
author.lastname=seq(23, by=6, length.out=7),
author.firstname=seq(24, by=6, length.out=7),
author.institution=seq(26, by=6, length.out=7),
author.city=seq(27, by=6, length.out=7),
author.country=seq(3, by=1, length.out=7)+1,
author.email=seq(25, by=6, length.out=7),
author.presenting=seq(22, by=6, length.out=7),
pres.title=18,
pres.abstract=21,
accept=17,
topic1=19,
topic2=3,
id=2, 
notes=73,
ref2=77,
ref3=78, 
ref4=79,
duplicated=NULL, #added May2011
accept.all=F, #added May2011,
#noNotes=F,   #added June2011
notes1=81,
notes2=86

)
{
#notes1: notes about the presentation day
#notes2: notes about the payments/registration


#my.filename: name of the tab-delimited file where databese with the abstracts is saved
#dirAbstracts: directory where the abstracts will be stored, it must exist

#get initial working directory
init.wd=getwd()


	my.data<-read.delim(my.filename, sep="\t")

#setwd("abstracts09try")
setwd(dirAbstracts)

#calculate the number of authors for each abstract
num.authors<-apply(my.data[,author.lastname], 1, function(x) sum(!is.na(x) & x!=""))

#remove the empty records
my.data<-my.data[num.authors!=0,]

#remove duplicaed records, if specified
####if(!is.null(duplicated)) my.data<-my.data[my.data[,duplicated]!=T,]


#if all must be accepted at this stage, set all as accepted - regulated from the accept.all=T parameter
#if(accept.all==T) my.data[,accept]<-"Yes"


#remove duplicated records and rejected papers, accepted paper must be indicated with "Yes" in the accept column, unless a full list is required, with the argument accept.all=T
if(accept.all!=T) my.data<-my.data[my.data[,accept]=="Yes",]

#calculate the number of authors
num.authors<-apply(my.data[,author.lastname], 1, function(x) sum(!is.na(x) & x!=""))
#calculate which is the presenting author, select just one if more than one was indicated as presenting
presenting.author<-as.numeric(unlist(apply(my.data[,author.presenting], 1, function(x) which(x=="Yes")[1])))
#the first author is the presenting author if none was indicated as presenting
presenting.author[is.na(presenting.author)]<-1


#initializing the vectors, different from generate.abstracts()
titolo<-autori<-affiliazioni<-rep(NA, dim(my.data)[1])

for(i in 1:dim(my.data)[1]){

	##########title##################################
	#titolo<-paste("{", my.data[i,pres.title]     , "}", sep="")
	titolo[i]<-paste("{\\bf ", my.data[i,pres.title]     , "}", sep="")
	###########end title#################################


	########### authors names##############################
	#getting author's initials
	TMP<-strsplit(as.character(my.data[,author.firstname[1]]), c(" "))
	#number of different names in first name
	number.of.names<- unlist(lapply(TMP, length))
	#derive initials of first names, used in the index
	authors.initials<-toupper(strsplit(TMP[[i]][1], "")[[1]][1]) 
	if(number.of.names[i]>1) for(j in 2:number.of.names[i])
	authors.initials<-paste(authors.initials, toupper(strsplit(TMP[[i]][j], "")[[1]][1]), sep="") 

	#determine if all the authors come from just one instutution, true if it is the same for all authors, false otherwise
	#will be used to write the insitution just once if TRUE
	one.inst.true<- length(unique(unlist((my.data[i,author.institution[1:num.authors[i]]]))))==1 & length(unique(unlist((my.data[i,author.city[1:num.authors[i]]]))))==1
	
	
	#derive a string containing the  author names,  as they will appear in the abstract
	#added a different way to report 1 single author, no underlying and no number 1
	if(num.authors[i]==1) {tmp<-paste(my.data[i, author.firstname[1] ], " ", my.data[i, author.lastname[1] ], "\\index{", 
my.data[i, author.lastname[1] ], ", ", authors.initials,   "}", sep="")} else {
if(presenting.author[i]==1) tmp<-paste("\\Presenting{", my.data[i, author.firstname[1] ], " ", my.data[i, author.lastname[1] ], "}", "$^1$", "\\index{", my.data[i, author.lastname[1] ], ", ", authors.initials,   "}", sep="") else tmp<-paste(my.data[i, author.firstname[1] ], " ", my.data[i, author.lastname[1] ], "$^1$", "\\index{", 
my.data[i, author.lastname[1] ], ", ", authors.initials,   "}", sep="")


	if(num.authors[i]>1)	for(j in 2:num.authors[i]){


	#getting author's initials
	TMP<-strsplit(as.character(my.data[,author.firstname[j]]), c(" "))
	number.of.names<- unlist(lapply(TMP, length))	
	#authors.initials<-toupper(unlist(lapply(TMP, function(x) strsplit(x, "")[[1]][1]))) 
	authors.initials<-toupper(strsplit(TMP[[i]][1], "")[[1]][1]) 
	if(number.of.names[i]>1) for(jj in 2:number.of.names[i])
	authors.initials<-paste(authors.initials, toupper(strsplit(TMP[[i]][jj], "")[[1]][1]), sep="") 
	if(j<num.authors[i]) {
		if(presenting.author[i]==j) tmp<-paste(tmp, ", \\Presenting{", paste(my.data[i, author.firstname[j] ], " ", my.data[i, author.lastname[j] ], "}$^", j, "$", "\\index{", my.data[i, author.lastname[j] ], ", ", authors.initials,   "}", 	sep=""), sep="")  else tmp<-paste(tmp, paste(my.data[i, author.firstname[j] ], " ", my.data[i, author.lastname[j] ], "$^", j, "$", "\\index{", my.data[i, author.lastname[j] ], ", ", authors.initials,   "}", 	sep=""), sep=", ") 
		} else {if(presenting.author[i]==j) tmp<-paste(tmp, "and \\Presenting{", paste(my.data[i, author.firstname[j] ], " ", my.data[i, author.lastname[j] ],  "}$^", j, "$",
"\\index{", my.data[i, author.lastname[j] ], ", ", authors.initials,   "}", 	sep=""), sep="")  else tmp<-paste(tmp, paste(my.data[i, author.firstname[j] ], " ", my.data[i, author.lastname[j] ],  "$^", j, "$", "\\index{", my.data[i, author.lastname[j] ], ", ", authors.initials,   "}", 	sep=""), sep=" and ")
}}
}# end else num.authors more than 1

	################authors################
	#autori<-paste("{", tmp, "}", sep="")
	autori[i]<-paste("{ ", tmp, "}", sep="")
	##################################
	
#####################end names of authors ###################################

	############affiliations###############
	# example of affiliations {\Afilliation{$^1$National Institute of Biology, Slovenia}; \Email{andrej.blejec@nib.si;} \Afilliation{$^2$University of Ljubljana, Slovenia}; #\Email{lara.lusa@mf.uni-lj.si}}
	if(num.authors[i]==1) {

	tmp<-paste("\\Afilliation{",  
		ifelse(my.data[i, author.institution[1] ]!="", paste(my.data[i, author.institution[1] ], ", ", sep=""), ""), 
		ifelse(my.data[i, author.city[1] ]!="", paste(my.data[i, author.city[1] ], ", ", sep=""), ""), 
		ifelse(my.data[i, author.country[1] ]!="", paste(my.data[i, author.country[1] ]), ""), 	
		"}\n", sep="")  } else 
		#\\Email{", 		ifelse(!is.na(my.data[i, author.email[1] ]),paste(my.data[i, author.email[1] ]),"") , 
		#"}\n", sep="")  } else 
	{
	if(one.inst.true){
		tmp<-paste("\\Afilliation{", 	
			ifelse(my.data[i, author.institution[1] ]!="", paste(my.data[i, author.institution[1] ], ", ", sep=""), ""), 
			ifelse(my.data[i, author.city[1] ]!="", paste(my.data[i, author.city[1] ], ", ", sep=""), ""), 
			ifelse(my.data[i, author.country[1] ]!="", paste(my.data[i, author.country[1] ]), ""), 
			"}\n", sep="")  #\\Email{", 
			#ifelse(!is.na(my.data[i, author.email[1] ]),paste(my.data[i, author.email[1] ]),"") , 
			

	#if(num.authors[i]>1)	
	for(j in 2:num.authors[i]){
		#tmp<-paste(tmp, "$^", j, "$", 
		#\\Email{", 			ifelse(!is.na(my.data[i, author.email[j] ]),paste(my.data[i, author.email[j] ]),"") , 
		#	"}\n", sep="")  
		#end for j} 
		} } else {
		tmp<-paste("\\Afilliation{$^", 1, "$", 
			ifelse(my.data[i, author.institution[1] ]!="", paste(my.data[i, author.institution[1] ], ", ", sep=""), ""), 
			ifelse(my.data[i, author.city[1] ]!="", paste(my.data[i, author.city[1] ], ", ", sep=""), ""), 
			ifelse(my.data[i, author.country[1] ]!="", paste(my.data[i, author.country[1] ]), ""), 
			"};\n", sep="")# \\Email{", 			ifelse(!is.na(my.data[i, author.email[1] ]),paste(my.data[i, author.email[1] ]),"") , 
			#"}\n", sep="")  


		#if(num.authors[i]>1)	
		for(j in 2:num.authors[i]){

			tmp<-paste(tmp, "\\Afilliation{$^", j, "$", 
				ifelse(my.data[i, author.institution[j] ]!="", paste(my.data[i, author.institution[j] ], ", ", sep=""), ""), 
				ifelse(my.data[i, author.city[j] ]!="", paste(my.data[i, author.city[j] ], ", ", sep=""), ""), 
				ifelse(my.data[i, author.country[j] ]!="", paste(my.data[i, author.country[j] ]), ""), 
				"}; \n", sep="")#\\Email{", 				ifelse(!is.na(my.data[i, author.email[j] ]),paste(my.data[i, author.email[j] ]),"") , 
				#"}\n", sep="")  
		}#end for j
	}# end else more than 1 affiliation

	}# end else more than 1 author for affiliation

	#####################affiliations######################
	#affiliazioni[i]<-paste("{", tmp, "}", sep="")
	affiliazioni[i]<-paste("{\\em", tmp, "}", sep="")
	#################################################


	####################abstracts############################
	#abstract[i]<-paste("{",  my.data[i,pres.abstract],  "}", sep="")
	######################################################
	


	########selected topics, abstract ID
	#temp<-paste("{Topic1: ",  my.data[i,topic1], ", Topic2: ",  my.data[i,topic2], ". Abstract ID: ", my.data[i,id],  "}", sep="")
# if(noNotes==F)	temp<-paste("{Topic1: ",  my.data[i,topic1], ", Topic2: ",  my.data[i,topic2], ". Abstract ID: ", my.data[i,id], ". Accepted: ", my.data[i, accept],   
#  ". Notes: ",  my.data[i,notes],  
#  ". Ref1: ", my.data[i, ref2],
#  ". Ref2: ", my.data[i, ref3],
#  ". Ref3: ", my.data[i, ref4],    "}", sep="") else temp<-paste("{Abstract ID: ", my.data[i,id], ". Topic1: ",  my.data[i,topic1], ", Topic2: ",  my.data[i,topic2],  ".}",  sep="")

} #end for i 



#generate the list of the submissions by topic

all.topics<-sort(unique(levels(my.data[,topic1]), levels(my.data[,topic2])))
#removing the empty topic
all.topics<-all.topics[all.topics!="---" | all.topics==""]

zz<-file("abstractListByTopic.tex", "w")

#browsing through topics
for( ii in 1:length(all.topics)){
	which.abstracts<-unique(which(my.data[,topic1]==all.topics[ii] | my.data[,topic1]==all.topics[ii]))

	#writing on file abstractListByTopic
	#zz<-file("abstractListByTopic.tex", "a")
	
	
	cat("{\\bf \\Large " ,  as.character(all.topics[ii]),    "}\\\\\\\\", sep=" ", file=zz)
	
	for(i in which.abstracts)
		cat(titolo[i], autori[i], affiliazioni[i], paste("ID=", my.data[i,id], "; Day=", my.data[i, notes1], "Payment=", my.data[i, notes2],  "\\\\ Topics={\\small ", as.character(my.data[i,topic1]), as.character(my.data[i,topic2]), "} \\\\\\\\", sep=" "),   sep="\\\\", file=zz)
	
cat("\\\\ \\clearpage", file=zz)

}# end for ii

	close(zz)
	




 


 
#  zz<-file(paste(my.data[i,id], ".tex", sep=""), "w")



  ##################write the abstract files, one file for each abstract, the abstract ID is used to name the files, 1.tex, 2.tex, etc#######################
#	zz<-file(paste(my.data[i,id], ".tex", sep=""), "w")
#	cat("\\A", titolo, autori, affiliazioni, temp, abstract, sep="\n", file=zz)
#	close(zz)
	##################end writing of files######################


	
	
	##############writes a file abstractList.tex 
	#zz<-file(paste("abstractList.tex", sep=""), "w")
	#for(i in my.data[,id])
#		cat("\\input{", i, ".tex}\n", sep="", file=zz)	
#	close(zz)

	



#restore initial working directory
setwd(init.wd)



}#########################end generate submission overview by topic#############################


	
	
	
	
	
	
	
#added Sept2011	
#support function to generate the correct order in the index of authors
#generates the {\index{Last Name, Initials}}  string
#if any of the characters listed in the my.char argument are present, it changes them in my.new.char
#this function is useful if some characters that are not correctly alphabetically sorted by LaTeX are present in the names of the authors
# example  \index{Zzizzek@?i?ek, S} example	for my.char="?" and my.new.char="Zz"

#2012, added some turkish characters "?"
.fun.get.index.slo<-function(my.lastname, my.initials, 
                             my.char=c("š", "ž", "č", "ć", "Š", "Ž", "Č", "Ć" ), 
                             my.new.char=c("szz", "zzz", "czz", "czzz", "SZZ", "ZZZ", "CZZ", "CZZZ")){
  #splits the last names in single letters
  #my.splitted<-unlist(strsplit(tolower(my.lastname), split="") )
####cat(as.character(my.lastname) )
 my.splitted<-unlist(strsplit(as.character(my.lastname), split=NULL) )
  
  #checks if any of the my.char are included in the last name
  my.char.present<-my.char %in% my.splitted
  
  if(any(my.char.present)) {
        #my.index<-paste("\\index{",  sub(my.char, my.new.char, (my.lastname)), "@", my.lastname, ", ", my.initials, "}", sep="")} else my.index<- paste("\\index{", my.lastname, ", ", my.initials, "}", sep="")
	for(ii in which(my.char.present)) {
	my.splitted[which(my.splitted%in% my.char[ii])]<-my.new.char[ii]
	}
	#recomposing the modified name in a string
	my.splitted<-sapply(list(my.splitted), paste, collapse="")

	my.index<-paste("\\index{",  my.splitted, "@", my.lastname, ", ", my.initials, "}", sep="")}  else my.index<- paste("\\index{", my.lastname, ", ", my.initials, "}", sep="")

        my.index
} # end .fun.get.index.slo
                                                       
                                                       
                                                       
#example of use 
#(.fun.get.index.slo("Ko?melj", "K"); )
#t(.fun.get.index.slo("Lusa", "L"))
#.fun.get.index.slo(my.lastname, my.intials)                                                                                      
# (.fun.get.index.slo("?iberna", "A") )
 
 
  

  #true if one of the my.char is present, true appears in the appropriate position  
  #my.char.present<-my.char %in% my.splitted

  #  for(i in which(my.char.present)){ 
  #    which.pos<-my.splitted %in% my.char[i] 
  #    my.splitted[which.pos]<-my.new.char[i]
   #    } #end for i
   #    
   #    }#and if(any)
   #    }#end function
       
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
######################## added September 2012 , function that writes a tex file with some information on the book content and style
	   
#' Function that defines the stlye used to generate the book of abstracts
#' @param dirAbstracts the path of the directory where the abstracts and the LaTeX templates are stored; the LaTeX file containing the definitions that define the style of the book of abstracts will be stored in this directory.
#' @param Style "AS2012" or "AS2011"; the two styles differ in the way the abstrats are formatted.
#' @param BookTitle a string indicating the title for the book of Program and Abstracts (for example: "Abstracts and program"), for Book.tex template 
#' @param BookSubmittedTitle a string indicating the title for the book of Submitted Abstracts (for example: "Submitted Abstracts"), for BookSubmitted.tex template
#' @param BookSubmittedAndAcceptedTitle a string indicating the title for the book of Program and Abstracts (for example: "Submitted Accepted Abstracts"), , for BookSubmitted.tex template 
#' @param DocYear Year when the conference is taking place, 
#' @param ConferenceTitle.line1 String containing the first line of conference title (for example: "International Conference")
#' @param ConferenceTitle.line2 String containing the first line of conference title (for example: "APPLIED STATISTICS")
#' @param Date String containing the date when the conference is taking place (for example, "September 25 - 28, 2011")
#' @param Place String containing the place where the conference is taking place (for example: "Ribno (Bled), Slovenia")
#' @param URL string containing the conference URL ("http://conferences.nib.si/AS2011")
#' @param Organizer String indicating the organizer of the conference ("Statistical Society of Slovenia
#' @param PublishedBy String that indicates who publishes the book, if more than a line is necessary include the "\\>" symbol at the beginning of each new line after the first, new lines are obtained adding "\\\\" at the end of the lines 
#' @param Editors String listing the editors of the book
#' @param Circulation Number of copies of the book
#' @param ISBN String with the ISBN of the conference ("978-961-92487-7-5")
#' @param ISBN2 String with an alternative ISBN of the conference ("978-961-92487-7-5"), defalult is empty string.
#' @param CIP String with the CIP number of the conference (example: 311(082.034.2))
#' @param Sponsors list containing the strings with the names of the sponsors; each sponsor will appear on a new line (for example: list("Slovenian Research Agency (ARSS)",  "Statistical Office of the Republic of Slovenia", "ALARIX", "RESULT d.o.o.", "VALICON / SPSS Slovenia", "ELEARN Web Services Ltd")) the output will be: "Slovenian Research Agency (ARSS) \\ [2mm] Statistical Office of the Republic of Slovenia\\[2mm] ALARIX\\[2mm] RESULT d.o.o.\\[2mm] VALICON / SPSS Slovenia\\[2mm] ELEARN Web Services Ltd\\[2mm]"
#' @param CenterPage2 a string containing what will appear in the center of page two. Can be a sentence - can be acknowledgements or Notes
#' @param BottomPage2 a string containing what will appear at the bottom of page two. Can be a sentence - can be acknowledgements or Notes, can also be a refenence to a file to input, as in our example
#' @param Footer a string containing the footer appearing in the book 
#' @param FigureFile string containing the name of the figure (in pdf format) to include on the cover of the book, must be in the same directory of the abstracts
#' @param copy.TeXTemplates logical, if set to TRUE the LaTeX templates provided with the package are copied in the dirAbstracts directory
#' @return empty return; TeX files are generated and stored dirAbstracts directory
#' @export
#' @examples 
#' set.seed(1)
#' @note The function writes the file called styleAndData.tex in the dirAbstracts directory. Two styles are avaialable, Style="AS2012" or Style="AS2011",  which differ in the way the abstracts are formatted.

write.styleAndData.tex=function(dirAbstracts, Book="Book_submitted", 
	Style="AS2012", 
	BookTitle="ABSTRACTS and PROGRAM",
	BookSubmittedTitle="Submitted ABSTRACTS",
	BookSubmittedAndAcceptedTitle="Accepted ABSTRACTS",
	DocYear="2012", 
	ConferenceTitle.line1="International Conference",
	ConferenceTitle.line2="APPLIED STATISTICS",
	Date="September 25 - 28, 2011", 
	Place="Ribno (Bled), Slovenia",
	URL="http://conferences.nib.si/AS2011", 
	Organizer="Statistical Society of Slovenia", 
	PublishedBy="Statistical Society of Slovenia\\\\
              \\>Vo\\v{z}arski pot 12\\\\
              \\> 1000 Ljubljana, Slovenia",
	Editors="Lara Lusa and Janez Stare",
	PrintedBy="Statistical Office of the Republic of Slovenia, Ljubljana",
	Circulation=200,
	ScientificComm=list("Janez Stare (Chair), Slovenia", "Toma\\v{z} Banovec, Slovenia", 
			"Vladimir Batagelj, Slovenia", "Jaak Billiet, Belgium", 
			"Maurizio Brizzi, Italy", "Brendan Bunting, Northern Ireland", 
			"Anu\\v{s}ka Ferligoj, Slovenia", "Herwig Friedl, Austria",
			"Dario Gregori, Italy", "Katarina Ko\\v{s}melj, Slovenia",
			"Dagmar Krebs, Germany", "Irena Kri\\v{z}man, Slovenia",
			"Lara Lusa, Slovenia", "Stanislaw Mejza, Poland",
			"Mihael Perman, Slovenia", "John O'Quigley, France",
			"Jo\\v{z}e Rovan, Slovenia" , "Tamas Rudas, Hungary",
			"Willem E. Saris, The Netherlands", "Albert Satorra, Spain", 
			"Vasja Vehovar, Slovenia" , "Hans Waege, Belgium"),
	OrganizingComm=list("Andrej Blejec (Chair)", "Bogdan Grmek", "Lara Lusa", "Anamarija Rebolj", "Irena Vipavc Brvar"),
	
	ISBN="978-961-92487-7-5", 
ISBN2="",                              
	CIP="311(082.034.2)",
	Sponsors=list("Slovenian Research Agency (ARSS)",  "Statistical Office of the Republic of Slovenia", "ALARIX", "RESULT d.o.o.", "VALICON / SPSS Slovenia", "ELEARN Web Services Ltd"),
	
	CenterPage2="The word cloud on the cover was generated using www.wordle.net. The source text included
the abstracts of the talks; the fifty most common words were displayed, and greater
prominence was given to words that appeared more frequently.",

	BottomPage2="\\input{CIP.tex}",
  #Footer="\\small{Applied Statistics}"
  Footer="Applied Statistics",
	FigureFile="FigureCover",
  copy.TeXTemplates=TRUE

	)
{

################# function that creates the file styleAndData.tex that is inputted in the LaTeX documents and defines the style of the abstracts and some important data about the book
### like the title, the date, etc


#dirAbstracts: the path of the directory where the abstracts and the LaTeX templates are stored
#Style="AS2012" or "AS2011",
#BookTitle: a string indicating the title for the book of Program and Abstracts (for example: "Abstracts and program"), for Book.tex template 
#BookSubmittedTitle: a string indicating the title for the book of Submitted Abstracts (for example: "Submitted Abstracts"), for BookSubmitted.tex template
#BookSubmittedAndAcceptedTitle: a string indicating the title for the book of Program and Abstracts (for example: "Submitted Accepted Abstracts"), , for BookSubmitted.tex template 
#DocYear=Year when the conference is taking place, 
#ConferenceTitle.line1=String containing the first line of conference title (for example: "International Conference")
#ConferenceTitle.line2=String containing the first line of conference title (for example: "APPLIED STATISTICS")
#Date=String containing the date when the conference is taking place (for example, "September 25 - 28, 2011")
#Place=String containing the place where the conference is taking place (for example: "Ribno (Bled), Slovenia")
#URL=String containing the conference URL ("http://conferences.nib.si/AS2011")
#Organizer=String indicating the organizer of the conference ("Statistical Society of Slovenia
#PublishedBy=String that indicates who publishes the book, if more than a line is necessary include the "\\>" symbol at the beginning of each new line after the first, new lines are obtained adding "\\\\" at the end of the lines 
#Editors=String listing the editors of the book
#Circulation=Number of copies of the book
#ISBN=String with the ISBN of the conference ("978-961-92487-7-5")
#ISBN2=String with the second ISBN of the conference ("978-961-92487-7-5 (pdf)")
#CIP=String with the CIP number of the conference (example: 311(082.034.2))
#Sponsors=list containing the strings with the names of the sponsors; each sponsor will appear on a new line (for example: list("Slovenian Research Agency (ARSS)",  "Statistical Office of the Republic of Slovenia", "ALARIX", "RESULT d.o.o.", "VALICON / SPSS Slovenia", "ELEARN Web Services Ltd"))
#the output will be: "Slovenian Research Agency (ARSS) \\ [2mm] Statistical Office of the Republic of Slovenia\\[2mm] ALARIX\\[2mm] RESULT d.o.o.\\[2mm] VALICON / SPSS Slovenia\\[2mm] ELEARN Web Services Ltd\\[2mm]"
#CenterPage2: a string containing what will appear in the center of page two. Can be a sentence - can be acknowledgements or Notes
#BottomPage2: a string containing what will appear at the bottom of page two. Can be a sentence - can be acknowledgements or Notes, can also be a refenence to a file to input, as in our example
#FigureFile: string containing the name of the figure (in pdf format) to include on the cover of the book, must be in the same directory of the abstracts, default is FigureCover.

#you can set equal to NULL all the components that you want to omit.

		#definition of a supporting function that writes on file the commands that define the new commands in the stlye file
		.write.newcommand=function(my.command, my.value, my.file){
			cat(paste("\\newcommand{", my.command, "}{", my.value, "}\n", sep=""), file=my.file)
			}  

		#definition of a supporting function that writes on file the commands that define the new commands in the stlye file, when data are from a list
		.write.newcommand.list=function(my.command, my.value, my.file, my.ncol=1){
		#ncol=number of columns in which to organize the data
			tmp.list=my.value[[1]]
			k=1
			#pasting the information from the list, taking care of the tabulation
			for(i in 2:length(my.value)) {if(k==my.ncol) {my.sep="\\\\"; k=1} else {my.sep=" \\> " ; k=k+1}
											tmp.list=paste(tmp.list, my.value[[i]], sep=my.sep)
											k}
			while(k<my.ncol) { 	tmp.list=paste(tmp.list, " \\> ")
								k=k+1}
			tmp.list=paste(tmp.list, "\\\\")
			.write.newcommand(my.command, tmp.list, zz)
			}  


			
		#save the working direcory
		initial.wd=getwd()
		#change the working directory to the directory where the abstracts are stored
		setwd(dirAbstracts)
		#define the file where the style will be written
		#if(Book=="Book_submitted")  zz=file(paste("styleAndData_Submitted.tex", sep=""), "w") else zz=file(paste("styleAndData.tex", sep=""), "w")
		zz=file(paste("styleAndData.tex", sep=""), "w")
		
		
		#begin writing
		.write.newcommand("\\Style", Style, zz)
		#titles of the books
		#book.tex
		.write.newcommand("\\DocTitle", BookTitle, zz)
		#book_submitted.tex
		.write.newcommand("\\DocTitleSubmitted", BookSubmittedTitle, zz)
		#book_submittedAndAccepted.tex
		.write.newcommand("\\DocTitleSubmittedAndAccepted", BookSubmittedAndAcceptedTitle, zz)
		#List_Complete_ByTopic
		.write.newcommand("\\DocTitleListByTopic", BookSubmittedAndAcceptedTitle, zz)
		#List_Accepted_ByTopic
		.write.newcommand("\\DocTitleListByTopicAccepted", BookSubmittedAndAcceptedTitle, zz)


.write.newcommand("\\DocYear", DocYear, zz)
.write.newcommand("\\DocConferenceTitleA", ConferenceTitle.line1, zz)
.write.newcommand("\\DocConferenceTitleB", ConferenceTitle.line2, zz)
.write.newcommand("\\DocDate", Date, zz)
.write.newcommand("\\DocPlace", Place, zz)
.write.newcommand("\\DocURL", URL, zz)
.write.newcommand("\\DocOrganizer", Organizer, zz)
.write.newcommand("\\DocPublisher", PublishedBy, zz)
.write.newcommand("\\DocEditors", Editors, zz)
.write.newcommand("\\DocPrinter", PrintedBy, zz)

.write.newcommand("\\DocCirculation", Circulation, zz)

.write.newcommand("\\DocISBN", ISBN, zz)
		.write.newcommand("\\DocISBNtwo", ISBN2, zz)
.write.newcommand("\\DocCIP", CIP, zz)
.write.newcommand("\\Footer", Footer, zz)


#sponsors can be a list, different way of writing the command, with separators that guarantee that each sponsor in given in a new line
if(length(Sponsors)==1) .write.newcommand("\\DocSponsors", Sponsors[[1]], zz)	else {
	tmp.sponsors=Sponsors[[1]]
	for(i in 2:length(Sponsors)) tmp.sponsors=paste(tmp.sponsors, Sponsors[[i]], sep="\\\\ [2mm]")
	tmp.sponsors=paste(tmp.sponsors, "\\\\ [2mm]")
	.write.newcommand("\\DocSponsors", tmp.sponsors, zz)
	}	

	
	.write.newcommand.list("\\DocScientificComm", ScientificComm, zz, my.ncol=2)
	.write.newcommand.list("\\DocOrganizingComm", OrganizingComm, zz, my.ncol=2)


	.write.newcommand("\\DocCenterPageTwo", CenterPage2, zz)
	.write.newcommand("\\DocBottomPageTwo", BottomPage2, zz)


	.write.newcommand("\\DocFigCover", FigureFile, zz)
	
	#close the file where the commands were written
	close(zz)



  #copy the TeX templates in the dirAbstracts directory if requested by the user (default)
  if(copy.TeXTemplates) {
    MySourceDir=file.path(path.package("generbook"),"extdata/TexTemplates")
    file.copy(list.files(MySourceDir, full.names=TRUE), dirAbstracts)
        
  }

	#reset the working direcory to the initial working directory
		setwd(initial.wd)


}#end of function write.styleAndData.tex







################## function that generates the list of all the authors, useful for the management of the registrations
generate.participants.list=function(
						my.filename, 
						author.lastname=seq(23, by=6, length.out=7),
						author.firstname=seq(24, by=6, length.out=7),
						author.institution=seq(26, by=6, length.out=7),
						author.city=seq(27, by=6, length.out=7),
						author.country=seq(3, by=1, length.out=7)+1,
						author.email=seq(25, by=6, length.out=7),
						author.presenting=seq(22, by=6, length.out=7),
						accept=17,
						id=2,
						notes=73){
						
	#read data from the submission database
	my.data<-read.delim(my.filename, sep="\t")

	#calculate the number of authors for each abstract
	num.authors<-apply(my.data[,author.lastname], 1, function(x) sum(!is.na(x) & x!=""))

	#remove the empty records
	my.data<-my.data[num.authors!=0,]

	
	#calculate the number of authors
	num.authors<-apply(my.data[,author.lastname], 1, function(x) sum(!is.na(x) & x!=""))
	#calculate which is the presenting author, select just one if more than one was indicated as presenting
	presenting.author<-as.numeric(unlist(apply(my.data[,author.presenting], 1, function(x) which(x=="Yes")[1])))
	#the first author is set as the presenting author if none was indicated as presenting
	presenting.author[is.na(presenting.author)]<-1

	#number of submitted papers
	num.submissions=nrow(my.data)			
	#maximum number of theoretical authors
	num.max.authors=length(author.lastname)
	
	#matrix that contains the indexes to use for each author, first row: for the first author, second row: second author, etc... 
	my.matrix.indexes=cbind(author.lastname,
						author.firstname,
						author.institution,
						author.city,
						author.country,
						author.email,
						author.presenting,
						rep(accept, num.max.authors),
						rep(id,num.max.authors),
						rep(notes, num.max.authors))
	   
	  
	 my.output=lapply(1:num.submissions, function(ii) (apply(my.matrix.indexes, 1,	function(index) my.data[ii,index])))

#	 my.data.new=my.data[1,my.matrix.indexes[,1]]
	 
#	 for(i in 1:num.submissions){
#		for(j in 1:num.max.authors){
#					my.data.new=rbind.data.frame(my.data.new, my.data[i, my.matrix.indexes[,j]]) 
#	 }}
	 
	 
#	 lapply(my.output, function(my.list) lapply(my.list, function(i) (as.data.frame(my..i)


	 
	my.output=  my.data

	   }