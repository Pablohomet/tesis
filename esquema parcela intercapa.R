require(grDevices)
## set up the plot region:
op <- par(bg = "thistle")
plot(c(100, 250), c(300, 450), type = "n", xlab = "", ylab = "",
     main = "2 x 11 rectangles; 'rect(100+i,300+i,  150+i,380+i)'")
i <- 4*(0:10)
## draw rectangles with bottom left (100, 300)+i
## and top right (150, 380)+i
rect(100+i, 300+i, 150+i, 380+i, col = rainbow(11, start = 0.7, end = 0.1))
rect(240-i, 320+i, 250-i, 410+i, col = heat.colors(11), lwd = i/5)
## Background alternating  ( transparent / "bg" ) :
j <- 10*(0:5)
rect(125+j, 360+j,   141+j, 405+j/2, col = c(NA,0),
     border = "gold", lwd = 2)
rect(125+j, 296+j/2, 141+j, 331+j/5, col = c(NA,"midnightblue"))
mtext("+  2 x 6 rect(*, col = c(NA,0)) and  col = c(NA,\"m..blue\")")

## an example showing colouring and shading
plot(c(00, 200), c(00, 150), type= "n", xlab = "", ylab = "")
points(15,25)
text(45,65, "45")
#arrows(x0=30,y0=20,x1=30,y1=20,length = 0.25)
#abline(a=NULL, 0, 30, 30 )
#abline(0,0,30,30)
abline(v=20)
abline(v=40)
abline(v=60)
abline(v=80)
abline(v=100)
abline(v=120)
abline(v=120)
abline(v=140)
abline(v=160)
abline(v=180)
abline(v=200)
abline(v=0)
abline(h=0)
abline(h=30)
abline(h=60)
abline(h=90)
abline(h=120)
abline(h=150)

#rect(100, 300, 125, 350) # transparent
#rect(100, 400, 125, 450, col = "green", border = "blue") # coloured
#rect(115, 375, 150, 425, col = par("bg"), border = "transparent")
#rect(150, 300, 175, 350, density = 10, border = "red")
#rect(150, 400, 175, 450, density = 30, col = "blue",
    # angle = -30, border = "transparent")

#legend(180, 450, legend = 1:4, fill = c(NA, "green", par("fg"), "blue"),
     #  density = c(NA, NA, 10, 30), angle = c(NA, NA, 30, -30))

#par(op)
