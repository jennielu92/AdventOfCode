## Day 1: The Tyranny of the Rocket Equation

CalculateFuel <- function(mass){
  fuel = floor(mass/3.0) - 2
  return(fuel)
}
CalculateFuelForFuel <- function(fuel){
  OriginalFuel = CalculateFuel(fuel)
  FuelAdd = CalculateFuel(CalculateFuel(fuel))
  FuelForFuel = 0
  while(FuelAdd>0){
    FuelForFuel = FuelForFuel + FuelAdd
    FuelAdd = CalculateFuel(FuelAdd)
  }
  return(FuelForFuel+OriginalFuel)
}

input = fread('Day1_inputs.txt',header = F, stringsAsFactors = FALSE,)

FuelList = sapply(input,CalculateFuel)
print(sum(FuelList))

TotalFuelList = sapply(input,CalculateFuelForFuel)
print(sum(TotalFuelList))

## Day 2: 1202 Program Alarm 
IntCode = function(sequence,input = 0){
  operation = 1
  while(operation <= length(sequence)){
    opcode = sequence[operation]
    ## op code
    if(opcode == 1){
      newval = sequence[sequence[operation+1]+1] + sequence[sequence[operation+2]+1]
      sequence[sequence[operation+3]+1] = newval
      operation = 4 + operation
    }
    else if(opcode == 2){
      newval = sequence[sequence[operation+1]+1] * sequence[sequence[operation+2]+1]
      sequence[sequence[operation+3]+1] = newval
      operation = 4 + operation
    }
    else if(opcode == 99){
      return(sequence[1])
      break
    }
  }
}

inputs = scan("Day2_inputs.txt", sep= ",")
IntCode(inputs)

## Changing address 1 and 2 to be [0,99], find address 0 = 19690720
seq1 = seq(0,99, by = 1)
seq2 = seq(0,99, by = 1)
for(i in 1:length(seq1)){
  for(j in 1:length(seq2)){
    sequence = inputs
    sequence[2] = i
    sequence[3] = j
    if(IntCode(sequence) == 19690720){
      print(sprintf("Address 1 is %i",i))
      print(sprintf("Address 2 is %i",j))
      print(IntCode(sequence))
      break
    }
  }
}

## Day 3: Crossed Wires
require('data.table')

ParseWirePaths = function(path){
  return(data.table(direction = sapply(path, function(x) substr(x,1,1)),
                    count = as.numeric(sapply(path, function(x) substr(x,2,nchar(x))))))
}
InitializeWireBox = function(Wire1,Wire2){
  Dim1 = sum(Wire1$count[which(Wire1$direction %in% c("R","L"))]) 
  Dim2 = sum(Wire1$count[which(Wire1$direction %in% c("U","D"))]) 
  return(matrix(0,nrow = (pmax(Dim1,Dim2)*2)+1,ncol = (pmax(Dim1,Dim2)*2)+1))
}
doAWire = function(WireBox,Wire1,Wire2){
  Dim1 = sum(Wire1$count[which(Wire1$direction %in% c("R","L"))]) 
  Dim2 = sum(Wire1$count[which(Wire1$direction %in% c("U","D"))]) 
  ## center of box, start there
  ydim = pmax(Dim1,Dim2) + 1
  xdim = pmax(Dim1,Dim2) + 1
  for(i in 1:nrow(Wire1)){
    if(Wire1$direction[i] == "R"){
      WireBox[ydim,(xdim + 1):(xdim + Wire1$count[i])] = 1
      xdim = xdim + Wire1$count[i]
    }
    if(Wire1$direction[i] == "L"){
      WireBox[ydim,(xdim - 1):(xdim - Wire1$count[i])] = 1
      xdim = xdim - Wire1$count[i]
    }
    if(Wire1$direction[i] == "U"){
      WireBox[(ydim - 1):(ydim -  Wire1$count[i]),xdim] = 1
      ydim = ydim - Wire1$count[i]
      
    }
    if(Wire1$direction[i] == "D"){
      WireBox[(ydim + 1):(ydim + Wire1$count[i]),xdim] = 1
      ydim = ydim + Wire1$count[i]
    }
  }
  return(WireBox)
}
ManhattanDistance = function(WireBox,Wire1,Wire2){
  Dim = pmax(sum(Wire1$count),sum(Wire2$count))
  
  possibleManhattan = data.frame(xdim = c(),
                                 ydim = c(),
                                 Distance = c())
  ## Where do wires cross?
  for(i in 1:(Dim*2)+1){
    for(j in 1:(Dim*2) + 1){
      if(WireBox[i,j] == 2){
        addme = data.frame(xdim = i, ydim = j, Distance = abs(i - Dim + 1) + abs(j - Dim + 1))
        possibleManhattan = rbind(possibleManhattan,addme)
        }
    }
  }
  return(possibleManhattan)
}

Input1 <- scan("Day3_inputs1.txt", what="", sep=",")
Input2 <- scan("Day3_inputs2.txt", what="", sep=",")

Wire1 = ParseWirePaths(Input1)
Wire2 = ParseWirePaths(Input2)

## wires originate at 0,0. Create matrix of zeros to be populated
WireBox = InitializeWireBox(Wire1,Wire2)
WirePath1 <- doAWire(WireBox, Wire1, Wire2) 
WirePath2 <- doAWire(WireBox, Wire2, Wire1)

WireBox = WirePath1 + WirePath2
md = ManhattanDistance(WireBox,Wire1,Wire2)
min(md$Distance)

## Day 5: Sunny with a Chance of Asteroids 

sequence <- as.numeric(strsplit(readLines("Day5_inputs.txt"), ",")[[1]])

IntCode = function(sequence,input = 0){
  operation = 1
  while(operation <= length(sequence)){
    full_opcode = sequence[operation]
    
    opcode <- as.character(full_opcode %% 100)
    
    ## get number of params for given opcode (https://github.com/mpjdem/adventofcode2019/blob/master/aoc19_day5.R)
    GetParams = function(opcode){
      if(opcode == "1"){return (3)}
      else if(opcode == "2"){return (3)}
      else if(opcode == "3"){return (1)}
      else if(opcode == "4"){return (1)}
      else if(opcode == "5"){return (2)}
      else if(opcode == "6"){return (2)}
      else if(opcode == "7"){return (3)}
      else if(opcode == "8"){return (3)}
      else if(opcode == "99"){return (0)}
    }
    
    par_modes <- sapply(10 ** (seq_len(GetParams(opcode)) + 1),
                       function(x) floor(full_opcode / x) %% 10)
    
    params = sequence[operation + seq_len(GetParams(opcode))]
    
    whichMode <- function(x) {
      if (par_modes[x] == 1) params[x] else sequence[params[x] + 1]
    }
    
    ## op code
    if(opcode == "1"){
      sequence[params[3] + 1] <- whichMode(1) + whichMode(2)
      operation = operation + GetParams(opcode) + 1
    }
    else if(opcode == "2"){
      sequence[params[3] + 1] <- whichMode(1) * whichMode(2)
      operation = operation + GetParams(opcode) + 1
    }
    else if(opcode == "3"){
      sequence[params[1] + 1] <- input
      operation = operation + GetParams(opcode) + 1
    }
    else if(opcode == "4"){
      ## output value at address sequence[operation+1]
      print(cat(whichMode(1), " "))
      operation = operation + GetParams(opcode) + 1
    }
    else if(opcode == "5"){
      if (whichMode(1) != 0){
        operation = whichMode(2) + 1
      }
      else{
        operation = operation + GetParams(opcode) + 1
      }
    }
    else if(opcode == "6"){

      if (whichMode(1) == 0){
        operation = whichMode(2) + 1
      }
      else{
        operation = operation + GetParams(opcode) + 1
      }
    }
    else if(opcode == "7"){
      sequence[params[3] + 1] <- ifelse(whichMode(1) < whichMode(2),1,0)
      operation = operation + GetParams(opcode) + 1
    }
    else if(opcode == "8"){
      sequence[params[3] + 1] <- ifelse(whichMode(1) == whichMode(2),1,0)
      operation = operation + GetParams(opcode) + 1
    }
    else if(opcode == "99"){
      #return(sequence[1])
      break
    }
  }
}

IntCode(sequence, input = 1)

IntCode(sequence, input = 5)

## Day 6: Universal Orbit Map
CalculateOrbits = function(solar_system){
  solar_system$center = sapply(strsplit(solar_system$orbits.V1,")"),"[",1)
  solar_system$orbiter = sapply(strsplit(solar_system$orbits.V1,")"),"[",2)
  all_planets = unique(c(solar_system$center,solar_system$orbiter))
  
  orbits = 0
  for(i in 1:length(all_planets)){
    planet = all_planets[i]
    while(planet!="COM"){
      planet = solar_system$center[which(solar_system$orbiter == planet)]
      orbits = orbits + 1
    }
  }
  return(orbits)
}
PathToCOM = function(solar_system,start_star){
  solar_system$center = sapply(strsplit(solar_system$orbits.V1,")"),"[",1)
  solar_system$orbiter = sapply(strsplit(solar_system$orbits.V1,")"),"[",2)

  path_to_COM = data.table(path = start_star)
  planet = start_star
  while(planet!="COM"){
    planet = solar_system$center[which(solar_system$orbiter == planet)]
    path_to_COM = rbind(path_to_COM, data.table(path = planet))
  }
  return(path_to_COM)
}
SmallestRoute = function(path1, path2){
  ## reorder
  path1 = path1[seq(nrow(path1),1),]
  path2 = path2[seq(nrow(path2),1),]
  
  ## ignore where paths converge
  common_path = 0
  for(i in 1:nrow(path1)){
    if(path1$path[i] == path2$path[i]){
      common_path = common_path + 1
    }
  }
  ## calculate number of movements to get to 
  ## the place where the paths converge
  moves = (nrow(path1) - common_path) + (nrow(path2) - common_path)
  
  ##adjustment for initial positions of YOU, SAN
  moves = moves - 2
  return(moves)
}
  
solar_system = data.table(orbits = read.delim('Day6_inputs.txt', header = FALSE, stringsAsFactors = FALSE))
CalculateOrbits(solar_system)

## Part 2: smallest orbital distance

YOU = PathToCOM(solar_system,start_star = "YOU")
SAN = PathToCOM(solar_system,start_star = "SAN")

SmallestRoute(YOU,SAN)


## Day 7: Amplification Circuit

IntCode = function(sequence,input = 0){
  operation = 1
  while(operation <= length(sequence)){
    full_opcode = sequence[operation]
    
    opcode <- as.character(full_opcode %% 100)
    
    ## get number of params for given opcode (https://github.com/mpjdem/adventofcode2019/blob/master/aoc19_day5.R)
    GetParams = function(opcode){
      if(opcode == "1"){return (3)}
      else if(opcode == "2"){return (3)}
      else if(opcode == "3"){return (1)}
      else if(opcode == "4"){return (1)}
      else if(opcode == "5"){return (2)}
      else if(opcode == "6"){return (2)}
      else if(opcode == "7"){return (3)}
      else if(opcode == "8"){return (3)}
      else if(opcode == "99"){return (0)}
    }
    
    par_modes <- sapply(10 ** (seq_len(GetParams(opcode)) + 1),
                        function(x) floor(full_opcode / x) %% 10)
    
    params = sequence[operation + seq_len(GetParams(opcode))]
    
    whichMode <- function(x) {
      if (par_modes[x] == 1) params[x] else sequence[params[x] + 1]
    }
    
    ## op code
    if(opcode == "1"){
      sequence[params[3] + 1] <- whichMode(1) + whichMode(2)
      operation = operation + GetParams(opcode) + 1
    }
    else if(opcode == "2"){
      sequence[params[3] + 1] <- whichMode(1) * whichMode(2)
      operation = operation + GetParams(opcode) + 1
    }
    else if(opcode == "3"){
      sequence[params[1] + 1] <- input
      operation = operation + GetParams(opcode) + 1
    }
    else if(opcode == "4"){
      ## output value at address sequence[operation+1]
      print(cat(whichMode(1), " "))
      operation = operation + GetParams(opcode) + 1
    }
    else if(opcode == "5"){
      if (whichMode(1) != 0){
        operation = whichMode(2) + 1
      }
      else{
        operation = operation + GetParams(opcode) + 1
      }
    }
    else if(opcode == "6"){
      
      if (whichMode(1) == 0){
        operation = whichMode(2) + 1
      }
      else{
        operation = operation + GetParams(opcode) + 1
      }
    }
    else if(opcode == "7"){
      sequence[params[3] + 1] <- ifelse(whichMode(1) < whichMode(2),1,0)
      operation = operation + GetParams(opcode) + 1
    }
    else if(opcode == "8"){
      sequence[params[3] + 1] <- ifelse(whichMode(1) == whichMode(2),1,0)
      operation = operation + GetParams(opcode) + 1
    }
    else if(opcode == "99"){
      #return(sequence[1])
      break
    }
  }
}

## Day 8: Space Image Format
require('stringr')
require('data.table')

space_image <- as.numeric(strsplit(readLines("Day8_inputs.txt"), "")[[1]])

RenderImage = function(input, width, height){
  image = data.table(contents=c())
  i = 1
  while(i <= length(input)){
    layer = data.table(i=input[i :(i + (width * height)-1)])
    image = cbind(image,layer) 
    i = i + (width * height)
  }
  return(image)
}
CountOccurences = function(image,countme=0){
  output = rep(0,each = ncol(image))
  for(i in 1:ncol(image)){
    output[i] = sum(str_count(image[[i]],as.character(countme)))
  }
  return(output)
}
EvaluateImage = function(image,occurences,dig1,dig2){
  min_occ = min(occurences)
  for(i in 1:length(occurences)){
    if(occurences[i] == min_occ){
      return(sum(str_count(image[[i]],as.character(dig1))) *
               sum(str_count(image[[i]],as.character(dig2)))) 
    }
  }
}
CreateImage = function(image, width, height){
  final_image = data.table(x = rep(seq(from = 1, to = width, length.out =width), times = height),
                           y = rep(seq(from = height, to = 1, length.out = height), each = width),
                           color = c(0))
  for(i in 1:nrow(final_image)){
    color_here = 2
    j = 1
    while(color_here == 2){
      color_here = image[[j]][i]
      j = j + 1
    }
    final_image$color[i] = color_here
  }
  return(final_image)
}

space_image = RenderImage(space_image,width = 25, height = 6)
zeros = CountOccurences(space_image,countme=0)

message = CreateImage(space_image,width = 25, height = 6)
p = ggplot(message,aes(x = x, y = y, fill = color)) +
  geom_tile() + 
  labs(x = NULL, y = NULL) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") 
ggsave(p, file = "elf_message.png",height = 4, width = 6)
