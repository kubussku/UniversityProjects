# Spiral matrix Generation

# Func Generating starting matrix, integer cellRange must be UNEVEN
StartMatrix = function(cellRange){
  if((cellRange %% 2) == 0){
    print("cellRange must be UNEVEN integer.")
  }
  else{
    return(matrix(1,cellRange,cellRange))      
  }

}
Matrix = StartMatrix(101)
# Starts in middle
Column_Position = round(ncol(Matrix)/2)
Row_Position = round(nrow(Matrix)/2)
# Starting parameters
Number = 1
Direction = factor('Right', levels = c('Right', 'Up','Left','Down'))
step = 0
Side_counter = 0
stepLeft = 1
# Sequence
for (i in c(1:(nrow(Matrix)*ncol(Matrix)))){
  Matrix[Row_Position,Column_Position] = Number
  # Position changer
  if(Direction == 'Right'){
    Column_Position<<-Column_Position+1
    Row_Position<<-Row_Position
  }
  if(Direction == 'Up'){
    Column_Position<<-Column_Position
    Row_Position<<-Row_Position-1
  }
  if(Direction == 'Left'){
    Column_Position<<-Column_Position-1
    Row_Position<<-Row_Position
  }
  if(Direction == 'Down'){
    Column_Position <<- Column_Position
    Row_Position <<- Row_Position+1
  }
  # If number of steps reaches side end. Direction is changed. Steps resets to 0 
  step = step + 1
  if (step == stepLeft){
    Side_counter <<- Side_counter + 1
    step = 0
    if(Direction == 'Right'){
      Direction = "Up"
    }else if(Direction == 'Up'){
      Direction = "Left"
    }else if(Direction == 'Left'){
      Direction = "Down"
    }else if(Direction == 'Down'){
      Direction = "Right"
    }
  }
  # Each second time direction is changed Steps that can be done rises by 1.
  if (Side_counter == 2){
    Side_counter<<-0 
    stepLeft<<-stepLeft +1
  }
  Number <<- Number + 1
}
Matrix


#___
mat = Matrix                                                                
# Starting Parameters
Column_Position = round(nrow(mat)/2)                                            
Row_Position = round(ncol(mat)/2)                                            
Time = 1                                                                  
# Data frame object for gathering results:
Results = data.frame(Column_Position,Row_Position,Time, Value = mat[Column_Position,Row_Position])   


#Sequence START HERE
while(Time < 2020){
  # Possible Moves generation
  Possible_Moves_func = function(){
    data.frame(
      Position_Names = c('Top_left',      
                         'Top_right',     
                         'Left_top',      
                         'Left_bottom',   
                         'Right_top',     
                         'Right_bottom',  
                         'Bottom_left',   
                         'Bottom_right'),
      Column_Positions = c((Column_Position - 1) ,
                           (Column_Position + 1) ,
                           (Column_Position - 2) ,
                           (Column_Position - 2) ,
                           (Column_Position + 2) ,
                           (Column_Position + 2) ,
                           (Column_Position - 1) ,
                           (Column_Position + 1)),
      Row_Positions = c((Row_Position - 2),
                        (Row_Position - 2),
                        (Row_Position - 1),
                        (Row_Position + 1),
                        (Row_Position - 1),
                        (Row_Position + 1),
                        (Row_Position + 2),
                        (Row_Position + 2))
    )
  }
  Possible_Moves = Possible_Moves_func()
  # Remove Moves which are beyond Matrix Scope
  Possible_Moves = Possible_Moves[Possible_Moves$Row_Positions %in% c(1:nrow(Matrix)) &Possible_Moves$Column_Positions %in% c(1:ncol(Matrix)),]
  # Get Values for possible moves
  Values = c()
  for(i in 1:nrow(Possible_Moves)){
    Values = append(Values,mat[Possible_Moves$Row_Positions[i],Possible_Moves$Column_Positions[i]])
  }
  Possible_Moves = cbind(Possible_Moves, Values)
  # Remove NA Values 
  Possible_Moves = na.omit(Possible_Moves)
  # Select Best Move
  Best_Move = Possible_Moves[Possible_Moves$Values == min(Possible_Moves$Values),]
  # Set Previous position value to NA (so knight wont return to that place anymore)
  mat[Row_Position,Column_Position] = NA
  # Set New Position 
  Column_Position = Best_Move$Column_Positions
  Row_Position =  Best_Move$Row_Positions
  Time = Time + 1
  
  Results = rbind(Results,
                  data.frame(Column_Position = Best_Move$Column_Positions,
                             Row_Position = Best_Move$Row_Positions,
                             Time,
                             Value = Best_Move$Value))
}

library(ggplot2)

ggplot(Results, aes(Column_Position, Row_Position, color = Time)) +
  geom_path() + theme_minimal() + scale_color_gradient2(mid = 'orange', low="yellow",high = "red") + 
  geom_point(aes(Column_Position[1],Row_Position[1])) + 
  geom_point(aes(Column_Position[2016],Row_Position[2016]))


#___ TESTS






