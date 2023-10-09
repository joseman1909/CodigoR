#a. Tiempo de vida de los sistemas
n = 1000
R1 = rlnorm(n, meanlog = 2, sdlog = 1)
R3 = rlnorm(n, meanlog = 2, sdlog = 1)
R2 = rlnorm(n, meanlog = 1, sdlog = 0.1)
R4 = rlnorm(n, meanlog = 1, sdlog = 0.1)

#sistema 1
s1_R12 = apply(data.frame(R1,R2),1,max)
s1_R34 = apply(data.frame(R3,R4),1,max)
sistema1 = apply(data.frame(s1_R12,s1_R34),1,min) 

#sistema 2
s2_R13 = apply(data.frame(R1,R3),1,min)
s2_R24 = apply(data.frame(R2,R4),1,min)
sistema2 = apply(data.frame(s2_R13,s2_R24),1,max)

#sistema 3
s3_R12 = apply(data.frame(R1,R2),1,min)
s3_R123 = apply(data.frame(s3_R12,R3),1,max)
sistema3 =  apply(data.frame(s3_R123,R4),1,min)

#b.
mean(sistema1)
mean(sistema2)
mean(sistema3)

#c.
plnorm(2, mean(sistema1), sd(sistema1), lower.tail = TRUE )
plnorm(2, mean(sistema2), sd(sistema2), lower.tail = TRUE )
plnorm(2, mean(sistema3), sd(sistema3), lower.tail = TRUE )

#d.
percentil20 <- qlnorm(0.2, meanlog = mean(sistema1), sdlog = sd(sistema1), lower.tail = TRUE)

#e.
dis_normal1 = dnorm(sistema1, mean(sistema1), sd(sistema1))
plot(sistema1, dis_normal1, ylab = "", xlab = "")

dis_normal2 = dnorm(sistema2, mean(sistema2), sd(sistema2))
plot(sistema2, dis_normal2, ylab = "", xlab = "")

dis_normal3 = dnorm(sistema3, mean(sistema3), sd(sistema3))
plot(sistema3, dis_normal3, ylab = "", xlab = "")

#f.
hist(sistema1,freq = FALSE)
hist(sistema2,freq = FALSE)
hist(sistema3,freq = FALSE)

