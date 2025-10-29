#######################################################################################################
# Plot the heatmap - all
for (decay in df_results$decay)
{
    # Take dec
    decay_data<-df_results[which(df_simulated_input_variables$decay==decay),]

    # Take time data
    decay_data$Time<-paste("Time_",decay_data$Time,sep="")
    
    # Set rownames
    rownames(decay_data)<-decay_data$Time

    # Remove row lines
    annotation_row_exp=decay_data[,c("n_discrete","BHP_discrete","H_discrete","operational_states","Diagnosis")]

    # Re-set colnmaes
    colnames(annotation_row_exp)<-c("n","BHP","H","operational_states","Diagnosis")

    # Specify colors
    ann_colors = list(n = c(Low="lightgrey", Medium="darkgrey",High="black"), BHP = c(Low="lightgrey", Medium="darkgrey",High="black"), H = c(Low="lightgrey", Medium="darkgrey",High="black") )

    # Normalized values for variables
    decay_data_discrete <- as.data.frame(lapply(decay_data[,c("Q","Tm.i","Tm.o","P1","P2","T","pi","mi","mo"),], tertile))

    # Add decay_data_discrete
    decay_data_discrete<-cbind(decay_data_discrete,annotation_row_exp)

    Diagnosis_rpart<-rpart(formula=operational_states ~ Q + Tm.i + Tm.o + P1 + P2 + T + pi + mi + mo, data=decay_data_discrete,method = "class")

    # Set rownames
    rownames(decay_data_normlized)<-rownames(decay_data)
  
    # bwplot               
    png(filename=paste(output_dir,paste("rpart_Operational_state_decay_",decay,".png",sep="")), width = 15, height = 15, res=600, units = "cm")  
      # Plot the bayesian network graph
      fancyRpartPlot(Diagnosis_rpart, caption = NULL, sub=NULL)  
    dev.off()
}
