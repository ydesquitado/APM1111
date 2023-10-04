normal <- c(67, 69, 
            70, 62, 
            63, 67, 
            65, 59, 
            68, 66, 
            60, 65, 
            70, 63, 
            64, 65, 
            69, 60, 
            61, 67, 
            66, 64, 
            65, 68, 
            71, 61, 
            62, 69, 
            66, 65, 
            68, 62, 
            64, 67, 
            67, 70, 
            62, 64, 
            66, 63, 
            65, 68, 
            63, 64, 
            66, 65, 
            65, 61, 
            63, 66)

skewRight <- c(31, 40, 
               43, 24, 
               30, 29, 
               30, 24, 
               38, 27, 
               26, 35, 
               29, 33, 
               55, 75, 
               46, 38, 
               26, 34, 
               29, 85, 
               57, 29, 
               34, 40, 
               34, 41, 
               36, 35, 
               40, 26, 
               28, 34, 
               26, 19, 
               66, 23, 
               63, 28, 
               30, 26, 
               33, 31, 
               24, 25, 
               35, 22, 
               34, 28)

skewLeft <- c(102, 87,
              55, 104,
              70, 75, 
              95, 80, 
              73, 66, 
              79, 93, 
              60, 90, 
              73, 84, 
              89, 73, 
              85, 98, 
              72, 79, 
              92, 35, 
              76, 71, 
              93, 90, 
              76, 71, 
              97, 63, 
              10, 58, 
              70, 82, 
              85, 72, 
              25, 93, 
              83, 44, 
              58, 65, 
              10, 77, 
              92, 81, 
              82, 77)

uniform <- c(12.1, 11.6, 
             12.1, 11.6, 
             12.4, 12.0, 
             12.1, 11.6, 
             12.1, 11.6, 
             12.2, 11.7, 
             12.2, 12.3, 
             12.2, 12.7, 
             11.9, 11.7, 
             12.2, 11.7, 
             12.3, 11.8, 
             12.3, 12.5, 
             11.7, 11.8, 
             12.3, 11.8, 
             12.3, 11.8, 
             12.4, 11.9,
             12.4, 11.9, 
             12.1, 11.9, 
             12.4, 12.2, 
             12.4, 11.9, 
             12.5, 12.0, 
             11.8, 11.9, 
             12.5, 12.0, 
             12.5, 12.0, 
             12.5, 12.0)

hist(normal, freq = FALSE)
lines(density(normal), col = "red")

hist(skewRight, freq = FALSE)
lines(density(skewRight), col = "red")

hist(skewLeft, freq = FALSE)
lines(density(skewLeft), col = "red")

hist(uniform, freq = FALSE)
lines(density(uniform), col = "red")

#####

#Normal (About 0)

normM1 <- sum(normal) / length(normal)
normM2 <- sum(normal^2) / length(normal)
normM3 <- sum(normal^3) / length(normal)
normM4 <- sum(normal^4) / length(normal)

print(paste0("First Moment of Normal is: ", normM1))
print(paste0("Second Moment of Normal is: ", normM2))
print(paste0("Third Moment of Normal is: ", normM3))
print(paste0("Fourth Moment of Normal is: ", normM4))

#####

# Skewed Right (About 0)

srM1 <- sum(skewRight) / length(skewRight)
srM2 <- sum(skewRight^2) / length(skewRight)
srM3 <- sum(skewRight^3) / length(skewRight)
srM4 <- sum(skewRight^4) / length(skewRight)

print(paste0("First Moment of Skewed Right is: ", srM1))
print(paste0("Second Moment of Skewed Right is: ", srM2))
print(paste0("Third Moment of Skewed Right is: ", srM3))
print(paste0("Fourth Moment of Skewed Right is: ", srM4))

#####

#Skewed Left (About 0)

slM1 <- sum(skewLeft) / length(skewLeft)
slM2 <- sum(skewLeft^2) / length(skewLeft)
slM3 <- sum(skewLeft^3) / length(skewLeft)
slM4 <- sum(skewLeft^4) / length(skewLeft)

print(paste0("First Moment of Skewed Left is: ", slM1))
print(paste0("Second Moment of Skewed Left is: ", slM2))
print(paste0("Third Moment of Skewed Left is: ", slM3))
print(paste0("Fourth Moment of Skewed Left is: ", slM4))

#####

# Uniform (About 0)

unifM1 <- sum(uniform) / length(uniform)
unifM2 <- sum(uniform^2) / length(uniform)
unifM3 <- sum(uniform^3) / length(uniform)
unifM4 <- sum(uniform^4) / length(uniform)

print(paste0("First Moment of Uniform is: ", unifM1))
print(paste0("Second Moment of Uniform is: ", unifM2))
print(paste0("Third Moment of Uniform is: ", unifM3))
print(paste0("Fourth Moment of Uniform is: ", unifM4))

#####

# Normal  (About Mean)

normM1M <- sum(normal - mean(normal)) / length(normal)
normM2M <- sum((normal - mean(normal))^2) / length(normal)
normM3M <- sum((normal - mean(normal))^3) / length(normal)
normM4M <- sum((normal - mean(normal))^4) / length(normal)

print(paste0("First Moment of Normal about the Mean is: ", normM1M))
print(paste0("Second Moment of Normal about the Mean is: ", normM2M))
print(paste0("Third Moment of Normal about the Mean is: ", normM3M))
print(paste0("Fourth Moment of Normal about the Mean is: ", normM4M))

#####

# Skewed Right (About Mean)

srM1M <- sum(skewRight - mean(skewRight)) / length(skewRight)
srM2M <- sum((skewRight - mean(skewRight))^2) / length(skewRight)
srM3M <- sum((skewRight - mean(skewRight))^3) / length(skewRight)
srM4M <- sum((skewRight - mean(skewRight))^4) / length(skewRight)

print(paste0("First Moment of Skewed Right about the Mean is: ", srM1M))
print(paste0("Second Moment of Skewed Right about the Mean is: ", srM2M))
print(paste0("Third Moment of Skewed Right about the Mean is: ", srM3M))
print(paste0("Fourth Moment of Skewed Right about the Mean is: ", srM4M))

#####

# Skewed Left (About Mean)

slM1M <- sum(skewLeft - mean(skewLeft)) / length(skewLeft)
slM2M <- sum((skewLeft - mean(skewLeft))^2) / length(skewLeft)
slM3M <- sum((skewLeft - mean(skewLeft))^3) / length(skewLeft)
slM4M <- sum((skewLeft - mean(skewLeft))^4) / length(skewLeft)

print(paste0("First Moment of Skewed Left about the Mean is: ", slM1M))
print(paste0("Second Moment of Skewed Left about the Mean is: ", slM2M))
print(paste0("Third Moment of Skewed Left about the Mean is: ", slM3M))
print(paste0("Fourth Moment of Skewed Left about the Mean is: ", slM4M))

#####

# Uniform (About Mean)

unifM1M <- sum(uniform - mean(uniform)) / length(uniform)
unifM2M <- sum((uniform - mean(uniform))^2) / length(uniform)
unifM3M <- sum((uniform - mean(uniform))^3) / length(uniform)
unifM4M <- sum((uniform - mean(uniform))^4) / length(uniform)

print(paste0("First Moment of Uniform about the Mean is: ", unifM1M))
print(paste0("Second Moment of Uniform about the Mean is: ", unifM2M))
print(paste0("Third Moment of Uniform about the Mean is: ", unifM3M))
print(paste0("Fourth Moment of Uniform about the Mean is: ", unifM4M))

#####

# Normal  (About 75)

normM175 <- sum(normal - 75) / length(normal)
normM275 <- sum((normal - 75)^2) / length(normal)
normM375 <- sum((normal - 75)^3) / length(normal)
normM475 <- sum((normal - 75)^4) / length(normal)

print(paste0("First Moment of Normal about the number 75 is: ", normM175))
print(paste0("Second Moment of Normal about the number 75 is: ", normM275))
print(paste0("Third Moment of Normal about the number 75 is: ", normM375))
print(paste0("Fourth Moment of Normal about the number 75 is: ", normM475))

#####

Moments = c("First Moment", "Second Moment", "Third Moment", "Fourth Moment", 
            "First Moment About the Mean", "Second Moment About the Mean", "Third Moment About the Mean", "Fourth Moment About the Mean", 
            "First Moment About the number 75", "Second Moment About the number 75", "Third Moment About the number 75", "Fourth Moment About the number 75")
Normal <- c(normM1, normM2, normM3, normM4, normM1M, normM2M, normM3M, normM4M, normM175, normM275, normM375, normM475)
Skewed_Right <- c(srM1, srM2, srM3, srM4, srM1M, srM2M, srM3M, srM4M, NA, NA, NA, NA)
Skewed_Left <- c(slM1, slM2, slM3, slM4, slM1M, slM2M, slM3M, slM4M, NA, NA, NA, NA)
Uniform <- c(unifM1, unifM2, unifM3, unifM4, unifM1M, unifM2M, unifM3M, unifM4M, NA, NA, NA, NA)

df <- data.frame(Moments, Normal, Skewed_Right, Skewed_Left, Uniform)

#####

print(paste0("(a) 2nd Moment about the Mean = 2nd Moment about the Number 75 - square of First Moment about the Number 75"))
print(paste0("(a) ", normM2M, " = ", normM275, " - (", normM175, ")^2"))
print(paste0("(a) ", normM2M, " = ", normM275, " - ", normM175^2))
print(paste0("(a) ", normM2M, " = ", normM275 - normM175^2))

print(paste0("(b) 3rd Moment about the Mean = 3rd Moment about the Number 75 - product of 3, 1st, and 2nd Moment about the Number 75, + product of 2 and cube of 1st Moment about 75"))
print(paste0("(b) ", normM3M, " = ", normM375, " - (3 * ", normM175, " * ", normM275, ") + (2 * (", normM175, "^3))"))
print(paste0("(b) ", normM3M, " = ", normM375, " - ", 3*normM175*normM275, " + ", 2*(normM175^3)))
print(paste0("(b) ", normM3M, " = ", normM375 - 3*normM175*normM275 + 2*(normM175^3)))

print(paste0("(c) 4th Moment about the Mean = 4th Moment about the Number 75 - product of 4, 1st, and 3rd Moment about the Number 75, + product of 6, square of 1st, and 2nd Moment about 75 - product of 3 and quartic of the 1st Moment about the Number 75"))
print(paste0("(c) ", normM4M, " = ", normM475, " - (4 * ", normM175, " * ", normM375, ") + (6 * ", normM175, "^2 * ", normM275, ") - (3 * ", normM475, "^4)"))
print(paste0("(c) ", normM4M, " = ", normM475, " - ", 4*normM175*normM375, " + ", 6*(normM175^2)*normM275, " - ", 3*(normM475^4)))
print(paste0("(c) ", normM4M, " = ", normM475 - 4*normM175*normM375 + 6*(normM175^2)*normM275 - 3*(normM175^4)))