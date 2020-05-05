library(igraph)
library(GGally)
library(network)
library(sna)
library(ggplot2)
library(intergraph)
library(hrbrthemes)
library(plotly)
library(ggplotify)

prog = function(name)
{
  
  #Reading data 
  data = read.csv(paste0("Data/", name , ".csv"))
  
  names(data) = c("Activity" ,"Title" ,"Immediate.Predecessor" , "Activity.Time")
  
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
  
  
  #Creating the start and End points ###########################################################################3
  start_node = "Start"
  start_nodes = data[data$Immediate.Predecessor == "-" , "Immediate.Predecessor"]
  
  r = data[1 , ]
  r$Activity = "Start"
  r$Title = 'Starting the project'
  r$Immediate.Predecessor = "-"
  r$Activity.Time = 0
  data[data$Immediate.Predecessor=="-","Immediate.Predecessor"] = "Start"
  data = rbind(r , data)
  
  end_nodes = unique(data[(! data$Activity %in% unique(data$Immediate.Predecessor) ), "Activity"] )
  
  for(i in end_nodes)
  {
    r = data[1 , ]
    r$Activity = "End"
    r$Title = 'Ending the project'
    r$Immediate.Predecessor = i
    r$Activity.Time = 0
    data = rbind(data , r)
  }
  end_node = "End"
  
  ##############################################################################################################
  
  #return parents of a node
  parents = function(data , node)
  {
    p = data[data$Activity ==node , "Immediate.Predecessor"]
    p
  }
  
  
  #return children of a node
  children = function(data , node)
  {
    c = data[data$Immediate.Predecessor == node , "Activity"]
    c
  }
  
  #return levels of the tree from a starting node
  levels = function(data , start)
  {
    queue = c(start)
    level = data.frame(Activity = unique(data$Activity) , level = rep(0 , length(unique(data$Activity))))
    level$Activity = as.character(level$Activity)
    while(length(queue) > 0)
    {
      node  =queue[1]
      queue = queue[-1]
      c = children(data , node)
      for(i in c)
      {
        level[level$Activity == i ,"level"]= max(level[level$Activity == node , "level"] + 1 ,  level[level$Activity == i ,"level"])
        queue=  c(queue , i)
      }
    }
    level
  }
  
  
  
  #Forward path 
  bfs = function (data )
  {
    level = levels(data, "Start")
    start = min(level$level)
    end = max(level$level)
    queue = c(start)
    path = vector()
    data$visited = rep(FALSE, nrow(data))
    data[data$Activity ==start , "visited"] = TRUE
    while(length(queue) > 0) 
    {
      node = queue[1]
      queue = queue[-1] 
      path = c(path , level[level$level ==node ,"Activity"] )
      c = level[level$level == node +1 , "Activity"]
      for(i in c) 
      {
        if(!data[data$Activity == i , "visited"])
        {
          p = parents(data , i)
          mx = max(data[data$Activity %in% p , "EF"])
          data[data$Activity ==i , "ES"] = mx 
          data[data$Activity ==i , "EF"]= mx+ data[data$Activity == i ,"Activity.Time"]
          data[data$Activity==i , "visited" ] = TRUE
        }
      }
      if(node<=end)
        queue = c(queue, node + 1)
    }
    l = list(data , path )
    l
  }
  
  #backward path 
  backward = function(data , path)
  {
    #Setting the Backward start node LS & LF
    data[data$Activity==end_node , c("LS" , "LF")] = c(data[data$Activity==end_node , c("ES" ,"EF")])
    
    path = rev(path )
    path = path[2:length(path)]
    for(i in path )
    {
      p = children(data , i )
      mn = min(data[data$Activity %in% p , "LS"])
      data[data$Activity == i , "LF" ] = mn 
      data[data$Activity == i , "LS"] = mn - data[data$Activity == i , "Activity.Time"]
    }
    data
  }
  
  
  
  #Creating the nodes (ES , EF , LS , LF  , visited status ) columns 
  data$ES = rep(0 , nrow(data))
  data$EF = rep(0 , nrow(data))
  data$LS = rep(0 , nrow(data))
  data$LF = rep(0 , nrow(data))
  
  
  #Setting the start node ES & EF
  data[data$Activity==start_node, c("ES" , "EF")] = c(0 , 0 )
  
  #Starting a forward bfs path 
  forward = bfs(data)
  data = forward[[1]]
  path = forward[[2]]
  
  data = data[ , !names(data) %in% "visited"]
  
  #Starting a backward bfs path
  data = backward(data , path )
  
  
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
  labels = c(labels , end_node)
  
  r = unique(data[2:nrow(data) , "Immediate.Predecessor"])
  r = c(r , end_node)
  
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
  net = graph.data.frame( df, directed = TRUE)
  
  file_name = paste0("images/", name , "/directed graph.png" )
  png(file_name , width = 1366 , height = 768 , units ="px")
  p = ggnet2 (net ,  arrow.size = 12 , arrow.gap = 0.055 , color = col , directed = T ,
              edge.label = data[2:nrow(data) , "S"] , 
              edge.label.fill = "#252a32" ,
              edge.label.color = "white" )+
    geom_point(aes(color = col), size = 12, alpha = 0.5) +
    geom_point(aes(color = col , Time = Time , ES = ES , EF = EF , LS = LS , LF = LF ), size = 9) +
    geom_text( label = labels  , color = "#000000", fontface = "bold"  ) +
    theme_ft_rc() +
    xlab("")+
    ylab("")+
    theme(legend.position = "none")
  
  print(p)
  dev.off()

  fig = ggplotly(p , tooltip = c("Time" , "ES" , "EF" , "LS" , "LF")   )
  axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
  fig <- fig %>% layout( xaxis = axis , yaxis = axis) 
  fig
  
  if(FALSE)
  {
    
    t = p$data
    fig = ggplotly(p , tooltip = c("Time" , "ES" , "EF" , "LS" , "LF")   )
    axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
    fig <- fig %>% layout( xaxis = axis , yaxis = axis) %>% 
      add_annotations(x = t$x + 0.02,
                      y = t$y + 0.02 ,
                      text = paste("Time : ", Time , "<br>" , "ES : " ,ES , "<br>" , "EF : " , EF  , "<br>" ,  "LS : " , LS , "<br>" , "LF : " , LF),
                      xref = "x",
                      yref = "y",
                      showarrow = TRUE,
                      arrowhead = 4,
                      arrowsize = .5,
                      ax = 20,
                      ay = -40 
      )
    fig
    
    
  }
  
  #Plot directed tree 
  
  layout <- layout.reingold.tilford(net)
  layout = layout[, 2:1]
  
  file_name = paste0("images/", name , "/directed tree.png" )
  
  png(file_name , width = 1366 , height = 768 , units ="px")
  p = plot(net,
           layout = layout,                   # draw graph as tree
           vertex.size = 20,                  # node size
           vertex.color = col,          # node color
           vertex.label = r ,        # node labels
           vertex.label.cex = 1.2,             # node label size
           vertex.label.family = "Helvetica", # node label family
           vertex.label.font = 2,             # node label type (bold)
           vertex.label.color = '#000000',    # node label size
           #edge.label = data[2:nrow(data) , "S"] ,          # edge label
           edge.label.cex = .7,               # edge label size
           edge.label.family = "Helvetica",   # edge label family
           edge.label.font = 2,               # edge label font type (bold)
           edge.label.color = '#000000',      # edge label color
           edge.arrow.size = 1.2,              # arrow size
           edge.arrow.width = 1.2              # arrow width
  ) 
  
  p
  dev.off()
  
}



prog("data")
prog("test1")
prog("test2")
prog("test3")


