library(igraph)
library(GGally)
library(network)
library(sna)
library(ggplot2)
library(intergraph)
library(hrbrthemes)
library(plotly)
#REading data 
data = read.csv("data.csv")

#Replacing the "factor" class of columns 
data$Immediate.Predecessor = as.character(data$Immediate.Predecessor)
data$Activity = as.character(data$Activity)


#Cleaning Immediate.Predecessor column from multible nodes
for(i in 1:nrow(data))
{
  if(substr(data[i, "Immediate.Predecessor"] , 2, 2 )== '-')
  {
    temp = strsplit(data[i , "Immediate.Predecessor"] ,"-")[[1]]
    data[i , "Immediate.Predecessor"]  = temp[1]
    for(j in 2:length(temp) )
    {
      r = data[i , ]
      r$Immediate.Predecessor = temp[j]
      data = rbind(data , r )
    }
  }
}



dfs = function(data, node , parent)
{
  current_node_time = data[data$Activity ==node & data$Immediate.Predecessor ==parent , "Activity.Time"] + 
    data[data$Activity ==node & data$Immediate.Predecessor ==parent , "ES"]
  children = data[data$Immediate.Predecessor == node , "Activity"]
  if(length(children) >=1)
  {
    
    for(i in children)
    {
      #print(i)
      if(data[data$Activity == i & data$Immediate.Predecessor ==node , "status"] == "N" )
      {
        data[data$Activity == i , "status"] = "V"
        data[data$Activity == i , "ES"] = current_node_time 
        data[data$Activity == i , "EF"] = current_node_time + data[data$Activity == i , "Activity.Time"]
        data = dfs(data , i , node )
      }
    }
  }
  data
}



bdfs = function(data , node )
{
  current_node_time = data[data$Activity ==node , "LS"] 
  if(length(current_node_time)>1)
    current_node_time = current_node_time[1]  
  parents = data[data$Activity == node , "Immediate.Predecessor"]
  parents = rev(parents)
  for(i in parents)
  {
    if(i != '-')
    for(indx in i)
    {
      print(paste(indx, current_node_time))
        if(data[data$Activity==indx, "status"] == "N" )
        {
          data[data$Activity==indx, "status"] = "V"
          data[data$Activity==indx , "LF"] = current_node_time 
          data[data$Activity==indx , "LS"] = abs(current_node_time -data[data$Activity==indx, "Activity.Time"])
          data = bdfs(data , i )
        } 
    }
  }
data
}



#Creating the nodes (ES , EF , LS , LF  , visited status ) columns 
data$ES = rep(0 , nrow(data))
data$EF = rep(0 , nrow(data))
data$LS = rep(0 , nrow(data))
data$LF = rep(0 , nrow(data))
data$status = rep("N" , nrow(data))

#Setting the start node ES & EF
data[data$Activity=="A", c("ES" , "EF")] = c(0 , 3)

#Starting a forward dfs path 
data = dfs(data , "A" , "-")

#Reset the visited column 
data$status = rep("N" , nrow(data))

#Setting the Backward start node LS & LF
data[data$Activity=="J" , c("LS" , "LF")] = c(data[data$Activity=="J" , c("ES" ,"EF")])

#Starting a backward dfs path
data = bdfs(data , "J" )

#dropping the visited status column 
data = data[, !names(data) %in% "status"]

#Creating "S" column = LS - ES for each node 
data$S = data$LS - data$ES

print(data[1:12 , -c(2) ])
#Creating a type column for each node to determine the Critical path 
data$type = rep("Critical" , nrow(data))

#Setting the type column based on the "S" Column 
for(i in 1:nrow(data))
{
  if(data[i, "S"] != 0)
    data[i, "type"] = "Normal"
}


#Plotting the graph 

df = data.frame(source= data$Immediate.Predecessor , target = data$Activity  , type = data$type )
df = df[2:nrow(df) , ]
df$type = data[1:(nrow(data)-1) , "type"]
network <- graph_from_data_frame(d=df, vertices = unique(data$Activity) ,  directed=T) 

col = vector()
labels = vector()
for(i in unique(data[2:nrow(data), "Immediate.Predecessor" ] )   )
{
  labels = c(labels , i)
  if(data[data$Activity == i, "type"]=="Critical")
    col = c(col , "#30d155")
  else
    col = c(col , "#ffd609")
}
col = c(col , "#30d155")
labels = c(labels , "J")

r = unique(data[2:nrow(data) , "Immediate.Predecessor"])
r = c(r , "J")

Time = vector()
ES = vector()
EF =  vector()
LS = vector()
LF = vector()
for(i in r )
{
  indx = which(data$Activity==i )
  if(length(indx) >1)
    indx = indx[1]
  Time = c(Time , data[indx  , "Activity.Time"])
  ES = c(ES , data[indx  , "ES"])
  EF = c(EF , data[indx  , "EF"])
  LS = c(LS , data[indx  , "LS"])
  LF = c(LF , data[indx  , "LF"])
}
net = graph.data.frame( df, directed = T)


p = ggnet2(net, arrow.size = 12 , arrow.gap = 0.055 , color = col , directed = T , edge.label = data[2:12 , "S"] , edge.label.fill = "#252a32" , edge.label.color = "white" )  +
  geom_point(aes(color = col), size = 12, alpha = 0.5) +
  geom_point(aes(color = col , Time = Time , ES = ES , EF = EF , LS = LS , LF = LF ), size = 9) +
  geom_text(label = labels ,  color = "white", fontface = "bold") +
  theme_ft_rc() +
  xlab("")+
  ylab("")+
  theme(legend.position = "none")

p
fig = ggplotly(p , tooltip = c("Time" , "ES" , "EF" , "LS" , "LF") , width = 1200 , height = 620 )
axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
fig <- fig %>% layout( xaxis = axis , yaxis = axis)
fig









