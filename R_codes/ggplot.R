# R for data science Book Exercises

library(tidyverse)
  
# Let's use the mpg dataset for cars to plot some graphs 
ggplot(data = mpg)+geom_point(mapping = aes(x=displ, y =hwy))
# The mpg dataset
mpg
# Column names in the mpg dataset
names(mpg)
ggplot(data = mpg)+geom_point(mapping = aes(x=displ,y=hwy,colour=class))
# Ordered class variables such as size have meaning to their values
# Unordered class variables such as class have no meaning to their values
# Categorical Variables are similar to ordered class variables

# Aesthetics are size shape alpha colour etc.
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy,size=class))
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy,shape=class))
# Set the aesthetic properties manually like colour of the whole plot

ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy),color="blue")
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy))+facet_wrap(~class)
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy))+facet_grid(drv~cyl)
# A geom is the geometrical object that a plot uses to represent data.
#People often describe plots by the type of geom that the plot uses.
#For example, bar charts use bar geoms, line charts use line geoms,
#boxplots use boxplot geoms, and so on.

ggplot(data=mpg)+geom_smooth(mapping=aes(x=displ,y=hwy),color="blue")
ggplot(data=mpg)+geom_smooth(mapping=aes(x=displ,y=hwy,linetype=drv),color="blue")
# Separates according to the drivetrain variable 
?mpg    
# Engine Displacement Highway Miles Per Gallon Drivetrain Number of cylinders
ggplot(data=mpg)+geom_smooth(mapping=aes(x=displ,y=hwy,group=drv),color="blue")  
ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy))+geom_smooth(mapping=aes(x=displ,y=hwy))
ggplot(data=mpg,mapping=aes(x=displ,y=hwy))+geom_point(mapping=aes(color=class))+geom_smooth()
ggplot(data=mpg,mapping=aes(x=displ,y=hwy))+geom_point(mapping=aes(color=class))+geom_smooth(data=filter(mpg,class=="subcompact"))

