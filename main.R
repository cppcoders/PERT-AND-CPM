library(igraph)

#REading data 
data = read.csv("data.csv")

#Replacing the "factor" class of columns 
data$Immediate.Predecessor = as.character(data$Immediate.Predecessor)
data$Activity = as.character(data$Activity)



#Cleaning Immediate.Predecessor column from multible nodes
for(i in 1:nrow(data))
{
  if( length(data$Immediate.Predecessor) >1 & substr(data[i, "Immediate.Predecessor"] , 2, 2 )== '-')
  {
    temp = as.character(substr(data[i, "Immediate.Predecessor" ], 3, 3) )
    temp2 = as.character(substr(data[i, "Immediate.Predecessor" ], 1, 1) )
    data[i , "Immediate.Predecessor"]  = temp2
    r = data[i , ]
    r$Immediate.Predecessor = temp 
    data = rbind(data , r )
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

#Creating a type column for each node to determine the Critical path 
data$type = rep("Critical" , nrow(data))
#Setting the type column based on the "S" Column 
for(i in 1:nrow(data))
{
  if(data[i, "S"] != 0)
    data[i, "type"] = "Normal"
}


















