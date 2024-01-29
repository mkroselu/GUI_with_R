


library(tcltk) ; library(tcltk2); library(tkrplot)

##### Basic Setup
### Input the food list

# change the path to where your save calorie.csv
imported_data <- read.csv (file = 'Desktop/calorie.csv')
attach(imported_data)

### Font
h1 = tkfont.create(size=12, weight="bold")
h2 = tkfont.create(size=10, weight="bold")
h3 = tkfont.create(size=17, weight="bold")
h4 = tkfont.create(size=12, weight="bold")

##### Graphical User Interface 

### Software Mainframe
tt <- tktoplevel()
tkwm.title(tt, "Calorie Calculation")

### Menu

TopMenu <- tkmenu(tt)
tkconfigure(tt, menu= TopMenu)

FileMenu <- tkmenu(TopMenu, tearoff= FALSE)
HelpMenu <- tkmenu(TopMenu, tearoff= FALSE)
tkadd(FileMenu, "command", label= "Quit", command= function() tkdestroy(tt))
tkadd(HelpMenu, "command", label= "How to use", command= function() tkmessageBox(message="Sorry! It's under construcion...", icon="error"))

tkadd(TopMenu, "cascade", label= "File", menu= FileMenu)
tkadd(TopMenu, "cascade", label= "Help", menu= HelpMenu)

### create 4 Interfaces

nb <- tk2notebook(tt, tabs = c("TDEE Calculation", "Meals", "Workout Suggestion", "Diet Suggestion"))
tkpack(nb, fill = "both", expand = 1)

tb1 <- tk2notetab(nb, "TDEE Calculation")
tb2 <- tk2notetab(nb, "Meals")
tb3 <- tk2notetab(nb, "Workout Suggestion")
tb4 <- tk2notetab(nb, "Diet Suggestion")

### TDEE Calculation (Interface 1)
## Input age, height, weight,gender and activity

tkgrid(tk2label(tb1, text = "Welcome to the Health Maintenance World!", font= h1), columnspan=3, pady=10)
tkgrid(tk2label(tb1, text = "First step: TDEE calculation. Please enter the following information.", justify = "left", font= h1), columnspan=3, pady=10)

# age
age <- tclVar("")
tb1$age <-tk2entry(tb1, width = "25", textvariable= age)
tkgrid(tk2label(tb1, text = "Age:", justify = "left"),
       padx = 10, pady = c(15, 5), sticky = "w")
tkgrid(tb1$age, padx = 10, pady = c(0, 15))

# height
height <- tclVar("")
tb1$height <-tk2entry(tb1, width = "25", textvariable= height)
tkgrid(tk2label(tb1, text = "Height (cm):", justify = "left"),
       padx = 10, pady = c(15, 5), sticky = "w")
tkgrid(tb1$height, padx = 10, pady = c(0, 15))

# weight
weight <- tclVar("")
tb1$weight <-tk2entry(tb1, width = "25", textvariable= weight)
tkgrid(tk2label(tb1, text = "Weight (kg):", justify = "left"),
       padx = 10, pady = c(15, 5), sticky = "w")
tkgrid(tb1$weight, padx = 10, pady = c(0, 15))

# gender
gender <- tclVar("")
tb1$gender <-tk2entry(tb1, width = "25", textvariable= gender)
tkgrid(tk2label(tb1, text = "Gender (M/F):", justify= "left"),
       padx = 10, pady = c(15, 5), sticky = "w")
tkgrid(tb1$gender, padx = 10, pady = c(0, 15))

# activity
activity <- tclVar("")
tkgrid(tk2label(tb1, text = "Activity:", justify = "left"),
       padx = 10, pady =c(15, 5), sticky = "w")

fre <- c("Sedentary (office job)", "Light Exercise (1-2 days/week)", "Medium Exercise (3-5 days/week)", "Heavy Exercise (6-7 days/week)", "Athlete (2x per day)")
tcombo <- tk2combobox(tb1, values = fre)
tkgrid(tcombo)
tkconfigure(tcombo, textvariable = activity)
tkinsert(tcombo, 0, "       -- Please choose --      ")

# goal
tkgrid(tk2label(tb1, text = "Goal:", justify = "left"),
       padx = 10, pady = c(15, 5), sticky = "w")
goal <- tclVar("0")
goals <- c("Gain weight", "Maintain weight", "Lose weight",
            "Build muscle", "Get rid of the fat")
tcombo.g <- tk2combobox(tb1, values = goals)
tkconfigure(tcombo.g, textvariable = goal)
tkgrid(tcombo.g)
tkinsert(tcombo.g, 0, "       -- Please choose --      ")

## Variables for goal calorie and three kinds of nutrient
calorie.goal <- tclVar("")
car.goal <- tclVar("")
pro.goal <- tclVar("")
fat.goal <- tclVar("")

## Calculate BMR & TDEE

TDEE <- function()
{
    ageVal <- as.numeric(tclvalue(age))
    heightVal <- as.numeric(tclvalue(height))
    weightVal <- as.numeric(tclvalue(weight))
    genderVal <- as.character(tclvalue(gender))
    activityVal <- as.character(tclvalue(activity))
    bmr <- switch(activityVal,
                  "Sedentary (office job)" = 1.2,
                  "Light Exercise (1-2 days/week)" = 1.375,
                  "Medium Exercise (3-5 days/week)" = 1.55,
                  "Heavy Exercise (6-7 days/week)" = 1.725,
                  "Athlete (2x per day)" = 1.9)
    tdee <- switch(genderVal,
                   "M" = (13.7*weightVal + 5*heightVal - 6.8*ageVal + 66)*bmr,
                   "F" = (9.6*weightVal + 1.8*heightVal - 4.7*ageVal + 655)*bmr)
        
    msg <- paste("Your original TDEE is:", tdee)
    tkgrid(tk2label(tb1, text = msg, justify = "left"))
        
    t1 <- "Actual calories you need to take to achieve goal is:"
        
    gain <- round(as.numeric(tdee)) + 300
    lose <- round(as.numeric(tdee)) - 300
    same <- round(as.numeric(tdee))
        
    gain.c <- round(0.65*gain/4)
    gain.p <- round(0.15*gain/4)
    gain.f <- round(0.2*gain/9)
        
    s.g.c <- paste("Carbohydrate", gain.c, "g")
    s.g.p <- paste("Protein", gain.p, "g")
    s.g.f <- paste("Fat", gain.f, "g")
    gain.all <- paste(s.g.c,"/", s.g.p,"/", s.g.f)
        
    lose.c <- round(0.65*lose/4)
    lose.p <- round(0.15*lose/4)
    lose.f <- round(0.2*lose/9)
    
    s.l.c <- paste("Carbohydrate", lose.c, "g")
    s.l.p <- paste("Protein", lose.p, "g")
    s.l.f <- paste("Fat", lose.f, "g")
    lose.all <- paste(s.l.c,"/", s.l.p,"/", s.l.f)
    
    same.c <- round(0.65*same/4)
    same.p <- round(0.15*same/4)
    same.f <- round(0.2*same/9)
    
    s.s.c <- paste("Carbohydrate", same.c, "g")
    s.s.p <- paste("Protein", same.p, "g")
    s.s.f <- paste("Fat", same.f, "g")
    same.all <- paste(s.s.c,"/", s.s.p,"/", s.s.f)
    
    muscle.f <- round(0.2*gain/9)
    muscle.p <- round(as.numeric(tclvalue(weight))*2)
    muscle.c <- round((as.numeric(tdee)*0.8 - muscle.p*4)/4)
    
    s.m.c <- paste("Carbohydrate", muscle.c, "g")
    s.m.p <- paste("Protein", muscle.p, "g")
    s.m.f <- paste("Fat", muscle.f, "g")
    muscle.all <- paste(s.m.c,"/", s.m.p,"/", s.m.f)
    
    rid.f <- round(0.2*lose/9)
    rid.p <- round(as.numeric(tclvalue(weight))*2.5)
    rid.c <- round((as.numeric(tdee)*0.8 - muscle.p*4)/4)
    
    s.r.c <- paste("Carbohydrate", rid.c, "g")
    s.r.p <- paste("Protein", rid.p, "g")
    s.r.f <- paste("Fat", rid.f, "g")
    rid.all <- paste(s.r.c,"/", s.r.p,"/", s.r.f)
    
        
    if(tclvalue(goal)=="Gain weight")(tkgrid(tk2label(tb1, text = paste(t1,gain,"kcal"), justify = "left")))
    if(tclvalue(goal)=="Gain weight")(tkgrid(tk2label(tb1, text = gain.all, justify = "left")))
    if(tclvalue(goal)=="Lose weight")(tkgrid(tk2label(tb1, text = paste(t1,lose,"kcal"), justify = "left")))
    if(tclvalue(goal)=="Lose weight")(tkgrid(tk2label(tb1, text = lose.all, justify = "left")))
    if(tclvalue(goal)=="Maintain weight")(tkgrid(tk2label(tb1, text = paste(t1,same,"kcal"), justify = "left")))
    if(tclvalue(goal)=="Maintain weight")(tkgrid(tk2label(tb1, text =same.all, justify = "left")))
    if(tclvalue(goal)=="Build muscle")(tkgrid(tk2label(tb1, text = paste(t1, gain, "kcal"), justify = "left")))
    if(tclvalue(goal)=="Build muscle")(tkgrid(tk2label(tb1, text = muscle.all, justify = "left")))
    if(tclvalue(goal)=="Get rid of the fat")(tkgrid(tk2label(tb1, text = paste(t1, lose, "kcal"), justify = "left")))
    if(tclvalue(goal)=="Get rid of the fat")(tkgrid(tk2label(tb1, text = rid.all, justify = "left")))
    
    tkgrid(tk2label(tb1, text = "Now you can go to next page, entering your meals.", justify = "left", font= h1))
        
    if(tclvalue(goal)=="Gain weight"){tclvalue(calorie.goal)=gain
                                      tclvalue(car.goal)=gain.c
                                      tclvalue(pro.goal)=gain.p
                                      tclvalue(fat.goal)=gain.f}
    if(tclvalue(goal)=="Lose weight"){tclvalue(calorie.goal)=lose
                                      tclvalue(car.goal)=lose.c
                                      tclvalue(pro.goal)=lose.p
                                      tclvalue(fat.goal)=lose.f}
    if(tclvalue(goal)=="Maintain weight"){tclvalue(calorie.goal)=same
                                          tclvalue(car.goal)=same.c
                                          tclvalue(pro.goal)=same.p
                                          tclvalue(fat.goal)=same.f}
    if(tclvalue(goal)=="Build muscle"){tclvalue(calorie.goal)=gain
                                       tclvalue(car.goal)=muscle.c
                                       tclvalue(pro.goal)=muscle.p
                                       tclvalue(fat.goal)=muscle.f}
    if(tclvalue(goal)=="Get rid of the fat"){tclvalue(calorie.goal)=lose
                                             tclvalue(car.goal)=rid.c
                                             tclvalue(pro.goal)=rid.p
                                             tclvalue(fat.goal)=rid.f}
                                            
    }
  
tb1$butOK1 <- tk2button(tb1, text = "OK", width = -6, command = TDEE)
tkgrid(tb1$butOK1, columnspan = 2, padx = 10, pady = c(5, 15))



### Meals Input (Interface 2)

tkgrid(tk2label(tb2, text = "Second step: Enter what you eat today.", font= h1),
       columnspan=3, pady=10)


## Classify food into six categories, set variable and produce combobox for each
# Grains
tkgrid(tk2label(tb2, text = " Grains"),row=1, column=0, pady=10)

grain <- tclVar("0")
grains <- as.character(imported_data[category=="grains",1])
tcombo.g <- tk2combobox(tb2, values = grains)
tkconfigure(tcombo.g, textvariable = grain)
tkgrid(tcombo.g)
tkinsert(tcombo.g, 0, "       -- Please choose --      ")

tkgrid(tk2label(tb2, text = "Amount"), row=1, column=1, pady=10)
amount.1 <- tclVar(0)
amount.1s <- 1:5
tcombo.1 <- tk2combobox(tb2, values = amount.1s)
tkconfigure(tcombo.1, textvariable = amount.1)
tkgrid(tcombo.1, row=2, column=1, pady=10)
tkinsert(tcombo.1, 0, "")


# Meat
tkgrid(tk2label(tb2, text = " Meat & Beans"), row=3, column=0, pady=10)
meat <- tclVar("0")
meats <- as.character(imported_data[category=="meat and beans",1])
tcombo.me <- tk2combobox(tb2, values = meats)
tkconfigure(tcombo.me, textvariable = meat)
tkgrid(tcombo.me)
tkinsert(tcombo.me, 0, "       -- Please choose --     ")

tkgrid(tk2label(tb2, text = "Amount"), row=3, column= 1, pady=10)
amount.2 <- tclVar(0)
amount.2s <- 1:5
tcombo.2 <- tk2combobox(tb2, values = amount.2s)
tkconfigure(tcombo.2, textvariable = amount.2)
tkgrid(tcombo.2, row=4, column=1)
tkinsert(tcombo.2, 0, "")

# Vegetable
tkgrid(tk2label(tb2, text = " Vegetable"), row=5, column=0, pady=10)
vegetable <- tclVar("0")
vegetables <- as.character(imported_data[category=="vegetable",1])
tcombo.v <- tk2combobox(tb2, values = vegetables)
tkconfigure(tcombo.v, textvariable = vegetable)
tkgrid(tcombo.v)
tkinsert(tcombo.v, 0, "       -- Please choose --     ")

tkgrid(tk2label(tb2, text = "Amount"), row=5, column= 1, pady=10)
amount.3 <- tclVar(0)
amount.3s <- 1:5
tcombo.3 <- tk2combobox(tb2, values = amount.3s)
tkconfigure(tcombo.3, textvariable = amount.3)
tkgrid(tcombo.3, row=6, column=1)
tkinsert(tcombo.3, 0, "")

# Fruit
tkgrid(tk2label(tb2, text = " Fruit"), row=7, column=0, pady=10)
fruit <- tclVar("0")
fruits <- as.character(imported_data[category=="fruits",1])
tcombo.fr <- tk2combobox(tb2, values = fruits)
tkconfigure(tcombo.fr, textvariable = fruit)
tkgrid(tcombo.fr)
tkinsert(tcombo.fr, 0, "       -- Please choose --     ")

tkgrid(tk2label(tb2, text = "Amount"), row=7, column= 1, pady=10)
amount.4 <- tclVar(0)
amount.4s <- 1:5
tcombo.4 <- tk2combobox(tb2, values = amount.4s)
tkconfigure(tcombo.4, textvariable = amount.4)
tkgrid(tcombo.4, row=8, column=1)
tkinsert(tcombo.4, 0, "")


# Milk 
tkgrid(tk2label(tb2, text = " Milk"), row=9, column=0, pady=10)
milk <- tclVar("0")
milks <- as.character(imported_data[category=="milk and dairy products",1])
tcombo.mi <- tk2combobox(tb2, values = milks)
tkconfigure(tcombo.mi, textvariable = milk)
tkgrid(tcombo.mi)
tkinsert(tcombo.mi, 0, "       -- Please choose --     ")

tkgrid(tk2label(tb2, text = "Amount"), row=9, column= 1, pady=10)
amount.5 <- tclVar(0)
amount.5s <- 1:5
tcombo.5 <- tk2combobox(tb2, values = amount.5s)
tkconfigure(tcombo.5, textvariable = amount.5)
tkgrid(tcombo.5, row=10, column=1)
tkinsert(tcombo.5, 0, "")


# Fat
tkgrid(tk2label(tb2, text = " Fat"), row=11, column=0, pady=10)
fat <- tclVar("0")
fats <- as.character(imported_data[category=="fats and oils",1])
tcombo.fa <- tk2combobox(tb2, values = fats)
tkconfigure(tcombo.fa, textvariable = fat)
tkgrid(tcombo.fa)
tkinsert(tcombo.fa, 0, "       -- Please choose --     ")

tkgrid(tk2label(tb2, text = "Amount"), row=11, column= 1, pady=10)
amount.6 <- tclVar(0)
amount.6s <- 1:5
tcombo.6 <- tk2combobox(tb2, values = amount.6s)
tkconfigure(tcombo.6, textvariable = amount.6)
tkgrid(tcombo.6, row=12, column=1)
tkinsert(tcombo.6, 0, "")


## Calorie calculation
# Variables for calorie calculation

calorie.ini = 0
calorie.var <- tclVar("0")

# Variables for food nutrient compositon calculation
carbo <- tclVar("0")
pro <- tclVar("0")
fa <- tclVar("0")

car.vec <- vector(mode = "numeric")
pro.vec <- vector(mode = "numeric")
fat.vec <- vector(mode = "numeric")




Calculation <- function(){
  
  grain.cal <- switch(tclvalue(grain),
                      "1 bowl of white rice (280)" = 280,
                      "1 white steamed bun (280)" = 280, 
                      "1 whole wheat steamed bun(210)" = 210, 
                      "1 corn (210)" = 210, 
                      "1 bowl of porridge (140)" = 140,
                      "1 bowl of glass noodles (140)" = 140,
                      "1 bowl of macaroni (140)" = 140, 
                      "1 bowl of sweet potato (140)" = 140, 
                      "1 bowl of taro (140)" = 140, 
                      "1 bowl of pumpkin (140)" = 140, 
                      "1 potato (140)" = 140, 
                      "1 bowl of white noodles (70)" = 70, 
                      "1 loaf of toast (70)" = 70,
                      "2 tbsp of red/green beans (70)" = 70,
                      "2 tbsp of glutinous rice balls(70)" = 70,
                      "2 tbsp of tapioca balls (70)" = 70,
                      "3 tbsp of oatmeal (70)" = 70,
                      0)
  
  meat.cal <- switch(tclvalue(meat),
                     "sugared soy milk (155)" = 155,
                     "beef brisket 50g (120)" = 120, 
                     "cod 50g (120)" = 120, 
                     "saury 50g (120)" = 120, 
                     "gluten 20g (120)" = 120, 
                     "a slice of cheese (120)" = 120, 
                     "3 pieces of short ribs (75)" = 75, 
                     "1 egg (75)" = 75, 
                     "1/2 box of tofu (75)" = 75, 
                     "edamame 45g (75)" = 75, 
                     "3 slices of dried tofu (75)" = 75, 
                     "2 pieces of fried tofu (75)" = 75, 
                     "milkfish 50g (75)" = 75, 
                     "2 table spoons of pork floss (75)" = 75, 
                     "chicken wings 35g (75)" = 75, 
                     "Sugar-free soy milk 260cc (55)" = 55, 
                     "chicken breast 50g (55)" = 55, 
                     "a slice of ham (55)" = 55, 
                     "10 clams (55)" = 55, 
                     "10 shrimps (55)" = 55, 
                     "beef tendon 50g (55)" = 55, 
                     "3 pieces of cuttlefish (55)" = 55, 
                     0)
  
  vegetable.cal <- switch(tclvalue(vegetable),
                          "Cabbage 100g (25)" = 25,
                          "Water spinach 100g (25)" = 25,
                          "Green pepper 100g (25)" = 25,
                          "Bitter melon 100g (25)" = 25,
                          "Napa cabbage 100g (25)" = 25,
                          "Bok choy 100g (25)" = 25,
                          "lettuce 100g (25)" = 25,
                          "luffa 100g (25)" = 25,
                          "yu choy 100g (25)" = 25,
                          "spoon cabbage 100g (25)" = 25,
                          "bean sprouts 100g (25)" = 25,
                          "chive 100g (25)" = 25,
                          "broccoli 100g (25)" = 25,
                          "bean sprouts 100g (25)" = 25,
                          "spinach 100g (25)" = 25,
                          "green beans 100g (25)" = 25,
                          "sweet potato leaf 100g (25)" = 25,
                          "enoki mushroom 100g (25)" = 25,
                          "enoki mushroom 100g (25)" = 25,
                          "shiitake mushroom 100g (25)" = 25,
                          "white mushroom(25)" = 25,
                          "bamboo 100g (25)" = 25,
                          0)
  
  fruit.cal <- switch(tclvalue(fruit),
                      "1 mango (240)" = 240,
                      "1 cantaloupe (240)" = 240,
                      "1 papaya (240)" = 240,
                      "1 banana (120)" = 120,
                      "1 guava (120)" = 120,
                      "1 persimmon (120)" = 120,
                      "1 starfruit (120)" = 120,
                      "1 small apple (60)" = 60,
                      "1 orange (60)" = 60,
                      "1 peach (60)" = 60,
                      "1 tangerine (60)" = 60,
                      "1 kiwi (60)" = 60,
                      "8 grapes (60)" = 60,
                      "1 pear (60)" = 60,
                      "9 cherries (60)" = 60,
                      "12 longans (60)" = 60,
                      "1 bowl of red watermelon (60)" = 60,
                      "15 small tomatoes (60)" = 60,
                      "1 jujube (60)" = 60,
                      "1 lychee (20)" = 20,
                      "1 wax apple (20)" = 20,
                      "1 piece of pomelo (20)" = 20,
                      0)
  
  milk.cal <- switch(tclvalue(milk),  
                     "4 spoons of whole milk powder (150)" = 150,
                     "3 spoons of low-fat milk powder(120)" = 120,
                     "Low fat fresh milk 240cc (120)" = 120,
                     "2 slices of low-fat cheese(120)" = 120,
                     "3 spoons of skimmed milk powder (80)" = 80,
                     0)
  
  fat.cal <- switch(tclvalue(fat),
                    "1/3 tsp of vegetable oil (45)" = 45,
                    "1/3 tsp of lard (45)" = 45,
                    "1/3 tsp of plant-based butter (45)" = 45,
                    "1 spoon of fresh cream (45)" = 45,
                    "2 tbsp of watermelon seeds(45)" = 45,
                    "10 pistachios (45)" = 45,
                    "5 almonds (45)" = 45,
                    "10 peanuts (45)" = 45,
                    "2 raw walnuts (45)" = 45,
                    "1/3 spoon of peanut powder (45)" = 45,
                    "1 spoon of almond flour (45)" = 45,
                    "2 tbsp of sesame seeds (45)" = 45,
                    "1/2 tsp of sesame paste (45)" = 45,
                    "1/2 tsp of shacha sauce (45)" = 45,
                    "1 slice of bacon (45)" = 45,
                    0)
  
  a1 <- as.numeric(tclvalue(amount.1))
  a2 <- as.numeric(tclvalue(amount.2))
  a3 <- as.numeric(tclvalue(amount.3))
  a4 <- as.numeric(tclvalue(amount.4))
  a5 <- as.numeric(tclvalue(amount.5))
  a6 <- as.numeric(tclvalue(amount.6))
  
  c1 <- as.numeric(grain.cal)
  c2 <- as.numeric(meat.cal)
  c3 <- as.numeric(vegetable.cal)
  c4 <- as.numeric(fruit.cal)
  c5 <- as.numeric(milk.cal)
  c6 <- as.numeric(fat.cal)
  
  p1 <- paste(tclvalue(grain), a1, a1*c1)
  p2 <- paste(tclvalue(meat), a2, a2*c2)
  p3 <- paste(tclvalue(vegetable), a3, a3*c3)
  p4 <- paste(tclvalue(fruit), a4, a4*c4)
  p5 <- paste(tclvalue(milk), a5, a5*c5)
  p6 <- paste(tclvalue(fat), a6, a6*c6)
  
  if(a1>=1 & a1<=5)(tkgrid(tk2label(tb2, text = p1), column= 0))
  if(a2>=1 & a2<=5)(tkgrid(tk2label(tb2, text = p2), column= 0))
  if(a3>=1 & a3<=5)(tkgrid(tk2label(tb2, text = p3), column= 0))
  if(a4>=1 & a4<=5)(tkgrid(tk2label(tb2, text = p4), column= 0)) 
  if(a5>=1 & a5<=5)(tkgrid(tk2label(tb2, text = p5), column= 0)) 
  if(a6>=1 & a6<=5)(tkgrid(tk2label(tb2, text = p6), column= 0))
  
  
  calorie.ini = calorie.ini + c1*a1 + c2*a2 +c3*a3 + 
    c4*a4 + c5*a5 + c6*a6
  
  tclvalue(calorie.var) = calorie.ini
  
  t1 <- "Calorie you take today is:"
  tkgrid(tk2label(tb2, text = t1, font= h1), row= 1, column= 2)
  tkgrid(tk2label(tb2, text = tclvalue(calorie.var), font= h3), row= 2, column= 2)
  
  # composition of three kinds of nutrients for six food categories
  # grain
  if(c1==280){car.vec[1]<- 60*a1 ; pro.vec[1]<- 8*a1}
  else 
    if(c1==210){car.vec[1]<- 45*a1 ; pro.vec[1]<- 6*a1}
  else 
    if(c1==140){car.vec[1]<- 30*a1 ; pro.vec[1]<- 4*a1} 
  else 
    if(c1==70){car.vec[1]<- 15*a1 ; pro.vec[1]<- 2*a1} 
  
  # meat 
  if(c2==155){car.vec[2]<- 25*a2 ; pro.vec[2]<- 7; fat.vec[2]<- 3*a2} 
  else 
    if(c2==120){pro.vec[2]<- 7*a2 ; fat.vec[2]<- 10*a2}
  else 
    if(c2==75){pro.vec[2]<- 7*a2 ; fat.vec[2]<- 5*a2}
  else 
    if(c2==55){pro.vec[2]<- 7*a2 ; fat.vec[2]<- 3*a2}
  
  # vegetable 
  if(c3==25){car.vec[3]<- 5*a3 ; pro.vec[3]<- 1*a3} 
  
  # fruit
  if(c4==240){car.vec[4]<- 60*a4} 
  else
    if(c4==120){car.vec[4]<- 30*a4} 
  else
    if(c4==60){car.vec[4]<- 15*a4} 
  else
    if(c4==20){car.vec[4]<- 5*a4} 
  
  # milk
  if(c5==150){car.vec[5]<- 12*a5 ; pro.vec[5]<- 8*a5 ; fat.vec[5]<- 8*a5}
  else
    if(c5==120){car.vec[5]<- 12*a5 ; pro.vec[5]<- 8*a5 ; fat.vec[5]<- 4*a5}
  else
    if(c5==80){car.vec[5]<- 12*a5 ; pro.vec[5]<- 8*a5}
  
  # fat
  if(c6==45){car.vec[6]<- 5*a6} 
  
  
  t2 <- "You eat Exceed: "
  t3 <- "You eat Beneath:"
  
  minus <- as.numeric(tclvalue(calorie.var))-as.numeric(tclvalue(calorie.goal))
  
  if(as.numeric(tclvalue(calorie.var)) > as.numeric(tclvalue(calorie.goal)))
  {tkgrid(tk2label(tb2, text = t2, font= h1), row= 3, column= 2)
    tkgrid(tk2label(tb2, text = minus, font= h3), row= 4, column= 2)}
  if(as.numeric(tclvalue(calorie.var)) < as.numeric(tclvalue(calorie.goal)))
  {tkgrid(tk2label(tb2, text = t3, font= h1), row= 5, column= 2)
    tkgrid(tk2label(tb2, text = minus, font= h3), row= 6, column= 2)}
  
  car.s <- sum(car.vec, na.rm = T)
  pro.s <- sum(pro.vec, na.rm = T)
  fat.s <- sum(fat.vec, na.rm = T)
  
  plotTk <- function(){

    slices <- c(car.s, pro.s, fat.s) 
    lbls <- c("Carbohydrate", "Protein", "Fat")
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, pct)
    lbls <- paste(lbls,"%",sep="")
    colors <- c("lightcoral", "antiquewhite", "firebrick")
    pie(slices,labels = lbls, col= colors,
        main="Your nutrient composition today:")
    
  }
  
  tt2 <- tktoplevel()
  tkwm.title(tt2, "Nutrient analysis")
  tt2$plot <- tkrplot(tt2, fun = plotTk)
  tkgrid(tt2$plot)
  
  ### Interface 3
  ## Input some common excercise
  excercise <- c("Badminton", "Tennis","Basketball(half)", 
                 "Volleyball", "Soccer", 
                 "Jog(8km/hr)", "Bicycle(10km/hr)")
  mets <- c(5.1, 6.6, 6.3, 3.6, 7.7, 8.2, 4)
  consume <- data.frame(excercise, mets)
  
  ## Excercise suggestion
  t10 <- "Today you exceeded: "
  minus <- as.numeric(tclvalue(calorie.var))-as.numeric(tclvalue(calorie.goal))
  p10 <- paste(t10, minus, "kcal")
  
  if(minus>0){
    tkgrid(tk2label(tb3, text = p10,font= h1), columnspan=3, pady=10)
    tkgrid(tk2label(tb3, text = "We suggest you do these excercises to burn the exeeded:",font= h1), columnspan=3, pady=10)
  }
  
  
  
  plotTk2 <- function(){
    badmi <- minus/as.numeric(tclvalue(weight))/consume[1,2]*60
    tenni <- minus/as.numeric(tclvalue(weight))/consume[2,2]*60
    bas.h <- minus/as.numeric(tclvalue(weight))/consume[3,2]*60
    volle <- minus/as.numeric(tclvalue(weight))/consume[4,2]*60
    socce <- minus/as.numeric(tclvalue(weight))/consume[5,2]*60
    joggi <- minus/as.numeric(tclvalue(weight))/consume[6,2]*60
    bike.s <- minus/as.numeric(tclvalue(weight))/consume[7,2]*60
    
    time.e <- c(badmi, tenni, bas.h, volle, socce, joggi, bike.s)
    color <- c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69')
    
    barplot (time.e, beside = T, main = "Excercise time",las = 0.3, xlab = "Time(min)",
             ylab = "Excercises", col=color, horiz= T, xpd= T)
    legend ("bottomright", legend = excercise, bty = 'n', pt.bg = color, col = "black",
            xpd= T, cex= 0.8)
    
  }
  
  
  tb3$plot2 <- tkrplot(tb3, fun = plotTk2, hscale=1, vscale=1)
  tkgrid(tb3$plot2)
  
  
  ### interface 4

  
  p20 <- car.s - as.numeric(tclvalue(car.goal))
  p21 <- pro.s - as.numeric(tclvalue(pro.goal))
  p22 <- fat.s - as.numeric(tclvalue(fat.goal))
  
  p23 <- paste("Carbohydrate:", p20)
  p24 <- paste("Protein:", p21)
  p25 <- paste("Fat:", p22)
  

  tkgrid(tk2label(tb4, text = "Nutrients you eat less or more:" ,font= h1), columnspan=3, pady=10)
  tkgrid(tk2label(tb4, text = p23),
         tk2label(tb4, text = p24),
         tk2label(tb4, text = p25),
         columnspan=1, pady=10)
  
  tkgrid(tk2label(tb4, text ="Healthy dish suggest:",font= h1), columnspan=3, pady=10)
  
  img <- tkimage.create("photo", file= "./data/food1.gif")
  tcanvas = tkcanvas(tb4, width=464, height=634, bd=0)
  bgnormal = tkcreate(tcanvas, "image", 0, 0, image= img, anchor="nw")
  
  tkpack.propagate(tcanvas, FALSE)
  tkgrid(tcanvas)
  
  
}


tb2$butOK1 <- tk2button(tb2, text = "Calculate", width = -6, command = Calculation)
tkgrid(tb2$butOK1, columnspan = 1, padx = 10, pady = c(5, 15)) 



### Workout suggestion (Interface 3)

tkgrid(tk2label(tb3, text = "Third step: This is workout suggestion for you according to WHAT you eat.",font= h1), columnspan=5, pady=10)


### Diet suggestion (Interface 4)

tkgrid(tk2label(tb4, text = "Fourth step: Suggestion for you to eat and live healthier.",font= h1), columnspan=5, pady=10)

