data<-DataSheet30_6
PQ1<-data$`10. How often have you been upset because of something that happened unexpectedly?`
PQ2<-data$`11. How often have you felt that you were unable to control the important things in your life?`
PQ3<-data$`12. How often have you felt nervous and stressed?`
PQ4<-data$`13. How often have you felt confident about your ability to handle your personal problems?`
PQ5<-data$`14. How often have you felt that things were going your way?`
PQ6<-data$`15. How often have you found that you could 0t cope with all the things that you had to do?`
PQ7<-data$`16. How often have you been able to control irritations in your life?`
PQ8<-data$`17. How often have you felt that you were on top of things?`
PQ9<-data$`18. How often have you been angered because of things that were outside of your control?`
PQ10<-data$`19. How often have you felt difficulties were piling up so high that you could 0t overcome them?`


library(dplyr)
df1= data.frame(PQ1,PQ2,PQ3,PQ6,PQ9,PQ10)

Factor1<- df1 %>%
  replace(is.na(.), 0) %>%
  mutate(factor1= rowSums(.[1:6]))

df2= data.frame(PQ4,PQ5,PQ7,PQ8)

Factor2<- df2 %>%
  replace(is.na(.), 0) %>%
  mutate(factor2= rowSums(.[1:4]))

DFSum<-data.frame(PQ1,PQ2,PQ3,PQ6,PQ9,PQ10,PQ4,PQ5,PQ7,PQ8)

TotalSum<- DFSum %>%
  replace(is.na(.), 0) %>%
  mutate(sum= rowSums(.[1:10]))



TotalSum$sum
#low stress
sum(TotalSum$sum >=0 & TotalSum$sum < 14)
#Moderate Stress
sum(TotalSum$sum >=14 & TotalSum$sum <=26)
#High Perceived Stress
sum(TotalSum$sum >=27 & TotalSum$sum <= 40)

#Spearman Corelation Coefficient
corr <- cor.test(x=TotalSum$PQ1, y=TotalSum$sum, method = 'spearman',exact=F)
cor.test(x=TotalSum$PQ2, y=TotalSum$sum, method = 'spearman',exact=F)
cor.test(x=TotalSum$PQ3, y=TotalSum$sum, method = 'spearman',exact=F)
cor.test(x=TotalSum$PQ4, y=TotalSum$sum, method = 'spearman',exact=F)
cor.test(x=TotalSum$PQ5, y=TotalSum$sum, method = 'spearman',exact=F)
cor.test(x=TotalSum$PQ6, y=TotalSum$sum, method = 'spearman',exact=F)
cor.test(x=TotalSum$PQ7, y=TotalSum$sum, method = 'spearman',exact=F)
cor.test(x=TotalSum$PQ8, y=TotalSum$sum, method = 'spearman',exact=F)
cor.test(x=TotalSum$PQ9, y=TotalSum$sum, method = 'spearman',exact=F)
cor.test(x=TotalSum$PQ10, y=TotalSum$sum, method = 'spearman',exact=F)



#Disribution of option scores in %

(sum(TotalSum$PQ1 == 0)/nrow(TotalSum))*100
(sum(TotalSum$PQ1 == 1)/nrow(TotalSum))*100
(sum(TotalSum$PQ1 == 2)/nrow(TotalSum))*100
(sum(TotalSum$PQ1 == 3)/nrow(TotalSum))*100
(sum(TotalSum$PQ1 == 4)/nrow(TotalSum))*100

(sum(TotalSum$PQ2 == 0)/nrow(TotalSum))*100
(sum(TotalSum$PQ2 == 1)/nrow(TotalSum))*100
(sum(TotalSum$PQ2 == 2)/nrow(TotalSum))*100
(sum(TotalSum$PQ2 == 3)/nrow(TotalSum))*100
(sum(TotalSum$PQ2 == 4)/nrow(TotalSum))*100

(sum(TotalSum$PQ3 == 0)/nrow(TotalSum))*100
(sum(TotalSum$PQ3 == 1)/nrow(TotalSum))*100
(sum(TotalSum$PQ3 == 2)/nrow(TotalSum))*100
(sum(TotalSum$PQ3 == 3)/nrow(TotalSum))*100
(sum(TotalSum$PQ3 == 4)/nrow(TotalSum))*100

(sum(TotalSum$PQ4 == 0)/nrow(TotalSum))*100
(sum(TotalSum$PQ4 == 1)/nrow(TotalSum))*100
(sum(TotalSum$PQ4 == 2)/nrow(TotalSum))*100
(sum(TotalSum$PQ4 == 3)/nrow(TotalSum))*100
(sum(TotalSum$PQ4 == 4)/nrow(TotalSum))*100

(sum(TotalSum$PQ5 == 0)/nrow(TotalSum))*100
(sum(TotalSum$PQ5 == 1)/nrow(TotalSum))*100
(sum(TotalSum$PQ5 == 2)/nrow(TotalSum))*100
(sum(TotalSum$PQ5 == 3)/nrow(TotalSum))*100
(sum(TotalSum$PQ5 == 4)/nrow(TotalSum))*100

(sum(TotalSum$PQ6 == 0)/nrow(TotalSum))*100
(sum(TotalSum$PQ6 == 1)/nrow(TotalSum))*100
(sum(TotalSum$PQ6 == 2)/nrow(TotalSum))*100
(sum(TotalSum$PQ6 == 3)/nrow(TotalSum))*100
(sum(TotalSum$PQ6 == 4)/nrow(TotalSum))*100

(sum(TotalSum$PQ7 == 0)/nrow(TotalSum))*100
(sum(TotalSum$PQ7 == 1)/nrow(TotalSum))*100
(sum(TotalSum$PQ7 == 2)/nrow(TotalSum))*100
(sum(TotalSum$PQ7 == 3)/nrow(TotalSum))*100
(sum(TotalSum$PQ7 == 4)/nrow(TotalSum))*100

(sum(TotalSum$PQ8 == 0)/nrow(TotalSum))*100
(sum(TotalSum$PQ8 == 1)/nrow(TotalSum))*100
(sum(TotalSum$PQ8 == 2)/nrow(TotalSum))*100
(sum(TotalSum$PQ8 == 3)/nrow(TotalSum))*100
(sum(TotalSum$PQ8 == 4)/nrow(TotalSum))*100

(sum(TotalSum$PQ9 == 0)/nrow(TotalSum))*100
(sum(TotalSum$PQ9 == 1)/nrow(TotalSum))*100
(sum(TotalSum$PQ9 == 2)/nrow(TotalSum))*100
(sum(TotalSum$PQ9 == 3)/nrow(TotalSum))*100
(sum(TotalSum$PQ9 == 4)/nrow(TotalSum))*100

(sum(TotalSum$PQ10 == 0)/nrow(TotalSum))*100
(sum(TotalSum$PQ10 == 1)/nrow(TotalSum))*100
(sum(TotalSum$PQ10 == 2)/nrow(TotalSum))*100
(sum(TotalSum$PQ10 == 3)/nrow(TotalSum))*100
(sum(TotalSum$PQ10 == 4)/nrow(TotalSum))*100

#**************************
library(psych)
#Communality --- h2 will be the Communality
communalityTotal<-factanal(DFSum, factors=2, rotation = "varimax")
Communality<- apply(communalityTotal$loadings^2,1,sum)

#****************
library(REdaS)
#Kaiser-Meyer-Olkin KMO
KMOS(DFSum, use = "pairwise.complete.obs")
#****************

bartlett.test(DFSum)


#Determinet Score
summary(DFSum)$r.squared 


#Confirmatory Factor Analysis (CFA) one factor 
#Confirmatory Factor Analysis (CFA)
require(lavaan)  #for doing the CFA
#require(semPlot)  #for plotting your CFA
require(psych)  #for calculating cronbach's alpha
require(dplyr)  #for subsetting data quickly when calculating cronbach's alpha

HS.model <-'question = ~ PQ1 + PQ2 + PQ3 + PQ6 + PQ9 + PQ10 + PQ4 + PQ5 + PQ7 + PQ8'
fit <-lavaan::cfa(HS.model,data=DFSum) # to avoid error 
summary(fit, fit.measures=TRUE)
fitmeasures(fit) 

#Confirmatory Factor Analysis (CFA) two factor 

HS.model <-'f1 = ~ PQ1 + PQ2 + PQ3 + PQ6 + PQ9 + PQ10
f2 = ~ PQ4 + PQ5 + PQ7 + PQ8'

fit <-lavaan::cfa(HS.model,data=DFSum) # to avoid error 
summary(fit, fit.measures=TRUE)
fitmeasures(fit) 

#Internal consistency and item discrimination
omega(df1)
omega(df2)
summary(cor(df1))



# Corelation between GAD-7 and PSS
#Factor1
F1Sum<- df1 %>%
  replace(is.na(.), 0) %>%
  mutate(sum= rowSums(.[1:6]))
F1Sum$sum
#Factor2        
F2Sum<- df2 %>%
        replace(is.na(.), 0) %>%
           mutate(sum= rowSums(.[1:4]))
F2Sum$sum
#TotalSum 
TotalSum<- DFSum %>%
  replace(is.na(.), 0) %>%
  mutate(sum= rowSums(.[1:10]))
TotalSum$sum


GadData<-coded_data_GAD7
g1<-GadData$`3. Feeling nervous, anxious, or on edge`
g2<-GadData$`4. 0t being able to stop or control worrying`
g3<-GadData$`5. Worrying too much about different things`
g4<-GadData$`6. Trouble relaxing`
g5<-GadData$`7. Being so restless that it's hard to sit still`
g6<-GadData$`8. Becoming easily an0yed or irritable`
g7<-GadData$`9. Feeling afraid as if something awful might happen`

GadSum_DF<-data.frame(g1,g2,g3,g4,g5,g6,g7)
GadTotalSum<- GadSum_DF %>%
  replace(is.na(.), 0) %>%
  mutate(sum= rowSums(.[1:7]))
GadTotalSum$sum

sum(GadTotalSum$sum >=5 & TotalSum$sum <= 9)
sum(GadTotalSum$sum >=10 & TotalSum$sum <= 14)
sum(GadTotalSum$sum >15)


#Corelation

#TotalPSS VS GAD7
cor.test(TotalSum$sum[1:657],GadTotalSum$sum,alternative = "greater")

#Factor1Total VS GAD7

cor.test(F1Sum$sum[1:657],GadTotalSum$sum)

#Factor2Total VS GAD7

cor.test(F2Sum$sum[1:657],GadTotalSum$sum)



####################################

##Degree Throws some error
## Vote rank for node specific items
## Pending

library(multinet)
library(stringr)
can_activate <-data.frame(actor=factor(),layer=factor())
new.df<-data.frame(actor=factor(),layer=factor(),Degree=numeric())
active_nodes <- data.frame(actor=factor(),layer=factor())
Seeds<-data.frame(actor=factor(),layer=factor())
result<-data.frame(network_name=factor(), Seed_Option=factor(), network=factor(), seed_count=numeric(), no_of_seeds=numeric(), no_of_activated=numeric(), no_of_iterations=numeric())
no_of_seeds<-factor()
interation_no <- 0
setwd("/Users/Research1723/Desktop/BelfinResearch/LayerSwitching_paper/Data/")

out.file<-""
file.names <- dir()

#results_ss <- data.frame(network_name=factor(), Seed_Option=factor(), network=factor(), pp=numeric(), network_id=numeric(), seed_count=numeric(), no_of_seeds=numeric(), no_of_activated=numeric(), no_of_iterations=numeric())
# # Loop Through Input Files
##for(i in 1:length(file.names))
#{#Start of File Loop
results_ss <- data.frame(network_name=factor(), Seed_Option=factor(), network=factor(), seed_count=numeric(), no_of_seeds=numeric(), no_of_activated=numeric(), no_of_iterations=numeric())
Data<-file.names[1]
  # Input_Network<-ml_aucs()
Input_Network <- read_ml(Data,sep = ",",aligned = F)
  network <- unlist(strsplit(Data, "_"))[1]

  for(seedPercent in c(0.02,0.05,0.10,0.20))
  {#Start of Seed % loop
    for(seedOption in c("random","degree","voteRank"))
    {#Start Seed Option Loop
      No_of_Seeds<-seedPercent*num_actors_ml(Input_Network, layers_ml(Input_Network))
      if(seedOption=="random")
      {
        NodeDF<-vertices_ml(Input_Network,layers_ml(Input_Network))
        Chosen_id<-sample(nrow(NodeDF), No_of_Seeds)
        seedList<-NodeDF[Chosen_id,]
      }
      else if(seedOption=="voteRank")
      {
        MlLayerList<-layers_ml(Input_Network)
        nodeList<-vertices_ml(Input_Network)
        actorList<-actors_ml(Input_Network)
        degreecalc<-list()
        SeedList<-list()
        #Tuple (Voting Score, #votes obtained)- (0,1)
        SelectedSeeds<-data.frame(actor=factor(),layer=factor())
        voteRankDF<-data.frame(nodes= nodeList$actor,layer=nodeList$layer,Score= c(rep(0,length(nodeList$actor))),votes=c(rep(1,length(nodeList$actor))))
        for(loop in 1:No_of_Seeds)
        {
          for(i in 1:length(voteRankDF$nodes))
          {
            neig<-neighbors_ml(Input_Network,as.character(voteRankDF$nodes[i]),
                               as.character(voteRankDF$layer[i]),"all")
            for(j in neig )
            {
              voteRankDF[which(voteRankDF$nodes==voteRankDF$nodes[i]
                               & voteRankDF$layer==voteRankDF$layer[i]),3] <-
                voteRankDF$Score[i]+voteRankDF$votes[i]
              
              
            }
          }
          degreecalc<-xneighborhood_ml(Input_Network,nodeList$actor,nodeList$layer,"all")
          Seed<-voteRankDF[which.max(voteRankDF$Score),1:2]
          SelectedSeeds<-rbind(SelectedSeeds,Seed)
          voteRankDF[which.max(voteRankDF$Score),3:4]<-0
          ReducingFactor<-1/mean(degreecalc)
          neighSeed<-xneighbors_ml(Input_Network,Seed$nodes,Seed$layer,"all")
          for(k in neighSeed)
          {
            voteRankDF[which(voteRankDF$nodes==k & voteRankDF$layer==Seed$layer),4]<-
              voteRankDF[which(voteRankDF$nodes==k & voteRankDF$layer==Seed$layer),4]-ReducingFactor
          }
        }
        seedList<-SelectedSeeds
      }
      else if(seedOption=="degree")
      {
        #Multiplex layer Degree?
        nodeList<-vertices_ml(Input_Network)
        neig1<-degree_ml(Input_Network,actors =nodeList$actor,layers = nodeList$layer,"all")
        top_actors<-new.df[order( new.df[,3] ),1:2]
        seedList<- tail(top_actors,No_of_Seeds)
      }
      else
      {
        break
      }
      for(threshold in c(0.1,0.2,0.3,0.5,0.8))
      {#Start of CascadeProb loop
        nodeList<-vertices_ml(Input_Network)
        LayerList<-layers_ml(Input_Network)
        seedOption<-seedOption
        #Seeds<-seedList
        can_activate <-Seeds
        active_nodes <- Seeds
        no_of_seeds<-nrow(Seeds)
        interation_no <- 0
        #while there is any node that can activate
        while (nrow(can_activate)>0) 
        {
          interation_no <- interation_no + 1
          print(interation_no)
          activated_nodes <- data.frame(actor=factor(),layer=factor())
          for(node in 1:nrow(can_activate))
          {
            #node_neigbours <- neighbors_ml(Input_Network)
            #total.neighbors <- length(node_neigbours)
            # activated.neighbors <- length(intersect(can_activate,node_neigbours))
            
            node_neigbours <- neighbors_ml(Input_Network,as.character(can_activate[node,1]),as.character(can_activate[node,2]))
            total.neighbors<-length(node_neigbours)
            activated.neighbors <- length(intersect(can_activate,node_neigbours))
            for(node_neigbour in node_neigbours)
            {
              currentLayer<-as.character(can_activate[node,2])
              node_neigbourDF<-data.frame(actor=node_neigbour,layer=currentLayer)
              if((!(node_neigbourDF$actor %in% active_nodes$actor & 
                    node_neigbourDF$layer %in% active_nodes$layer)) && 
                 activated.neighbors * 1.0 / total.neighbors >= threshold)
              {
                activated_nodes <- rbind(activated_nodes, node_neigbourDF)
                print(activated_nodes)
                active_nodes <- rbind(active_nodes, node_neigbourDF)
                print(active_nodes)
              }
            }
          }
          can_activate <- activated_nodes
          result <- data.frame(network_name=Data,
                               Seed_Option=seedOption, 
                               network=network, 
                               seed_count= seedPercent, 
                               no_of_seeds= nrow(Seeds), 
                               no_of_activated=length(unique(active_nodes$actor)), 
                               no_of_iterations=interation_no)
          
          results_ss <- rbind(results_ss, result)
        }
      }#End of Cascade Prob loop
      results_ss <- rbind(results_ss, result)
    }#End Seed option Loop
    
  }#End of Seed % loop
  file_path <- "/Users/Research1723/Desktop/BelfinResearch/LayerSwitching_paper/output/LTMNodeSpecific/"
  save_path<- paste(file_path,network,"_LTM_Layer_Switch_output.txt", sep="")
  write.csv(results_ss,save_path)
#}#End of File Loop



