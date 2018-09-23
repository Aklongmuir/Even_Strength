################################################################################
##This code written by Prashanth Iyer who can be found on twitter          #####
##@iyer_prashanth and is a really good follow                              #####
################################################################################
fun.draw_rink <- function() {
    
    
    
    xseq <- seq(-4, 4, length = 100)
    theta1 <- seq(0, 2 * pi, length = 300)
    theta <- seq(0, 2 * pi, length = 300)
    dd <- (5 + 7 / 12) / 2
    
    ## Blank NHL Rink
    
    rink <- ggplot(data = data.frame(x = 1, y = 1), aes(x, y)) + 
        
    geom_path(data = data.frame(
        x = c(15, 87 + 13 * sin(seq(0, pi / 2, length = 20)), 
            87 + 13 * sin(seq(pi / 2, 0, length = 20)), 15), 
        y = c(-42.5, -42.5 + 15 - 15 * cos(seq(0, pi / 2, length = 20)), 
            42.5 - 15 + 15 * cos(seq(pi / 2, 0, length = 20)), 42.5))) + 
    geom_path(data = data.frame(
        x = c(15, -87 - 13 * sin(seq(0, pi / 2, length = 20)), 
            -87 - 13 * sin(seq(pi / 2, 0, length = 20)), 15), 
        y = c(-42.5, -42.5 + 15 - 15 * cos(seq(0, pi / 2, length = 20)), 
            42.5 - 15 + 15 * cos(seq(pi / 2, 0, length = 20)), 42.5))) + 
    ## Goal Lines
    geom_path(data = data.frame(x = c(89),
                                y = c(42.5 - 15 + sqrt(15^2 - (15 - 11)^2), 
                                    -(42.5 - 15 + sqrt(15^2 - (15 - 11)^2)))), 
              color = 'red') + 
    geom_path(data = data.frame(x = c(-89), 
                                y = c(42.5 - 15 + sqrt(15^2 - (15 - 11)^2), 
                                    -(42.5 - 15 + sqrt(15^2 - (15 - 11)^2)))), 
              color = 'red') +
    ## Nets
    geom_path(data = data.frame(x = c(90, 92, 92, 90)), y = c(-3, -3, 3, 3)) + 
    geom_path(data = data.frame(x = c(-90, -92, -92, -90), y = c(-3,-3, 3, 3))) +
    
    ## Restricted Area
    geom_segment(aes(x = 89, y = -11, xend = 100, yend = -14), color = 'red') + 
    geom_segment(aes(x = 89, y = 11, xend = 100, yend = 14), color = 'red') + 
    geom_segment(aes(x = -89, y = -11, xend = -100, yend = -14), color = 'red') + 
    geom_segment(aes(x = -89, y = 11, xend =-100, yend = 14), color = 'red') +
        
    ## Red Line (Center Ice)
    geom_segment(aes(x = 0, y = -42.5, xend = 0, yend = 42.5), color = 'red', size = 1) +
    
    ## Blue Lines
    geom_segment(aes(x = 25, y = -42.5, xend = 25,  yend = 42.5), color = 'blue', size = 1) + 
    geom_segment(aes(x = -25, y = -42.5, xend = -25,  yend = 42.5), color = 'blue', size = 1) +
        
    ## Crease
    geom_polygon(data = data.frame(x = 1 * c(89, 83+xseq^2 / 4^2 * 1.5, 89),
                                   y = c(-4, xseq, 4)), 
                 color = 'red', fill = 'deepskyblue2') + 
    geom_polygon(data = data.frame(x = -1 * c(89, 83 + xseq^2 / 4^2 * 1.5, 89),
                                   y = c(-4, xseq, 4)), 
                 color = 'red', fill = 'deepskyblue2') +

    ## Center Ice Circle
    geom_path(data = data.frame(x = 15 * sin(theta1)), 
              y = 15 * cos(theta1), color = 'deepskyblue2') +
        
    ## Faceoff Dots
    geom_polygon(data = data.frame(y = 22 + 1 * cos(theta), 
                                   x = 20 + 1 * sin(theta)), 
                 color = "red", fill = "red") + 
    geom_polygon(data = data.frame(y = 22 + 1 * cos(theta), 
                                   x = -20 + 1 * sin(theta)), 
                 color = "red", fill = 'red') + 
    geom_polygon(data = data.frame(y = -22 + 1 * cos(theta), 
                                   x = -20 + 1 * sin(theta)), 
                 color = 'red', fill = 'red') + 
    geom_polygon(data = data.frame(y = -22 + 1 * cos(theta), 
                                   x = 20 + 1 * sin(theta)), 
                 color = 'red', fill = 'red') + 
    geom_polygon(data = data.frame(y = 22 + 1 * cos(theta), 
                                   x = -69 + 1 * sin(theta)), 
                 color = 'red', fill = 'red') + 
    geom_polygon(data = data.frame(y = 22 + 1 * cos(theta), 
                                   x = 69 + 1 * sin(theta)), 
                 color = 'red', fill = 'red') + 
    geom_polygon(data = data.frame(y = -22 + 1 * cos(theta), 
                                   x = -69 + 1 * sin(theta)), 
                 color = 'red', fill = 'red') + 
    geom_polygon(data = data.frame(y = -22 + 1 * cos(theta), 
                                   x = 69 + 1 * sin(theta)), 
                 color = 'red', fill = 'red') +

    ## Faceoff Circles
    geom_segment(aes(y = 22 - 0.75, x = 69 - 2, 
                     yend = 22 - 0.75, xend = 69 - 6), color = 'red') + 
    geom_segment(aes(y = 22 + 0.75, x = 69 - 2, 
                     yend = 22 + 0.75, xend = 69 - 6), color = 'red') + 
    geom_segment(aes(y = 22 + 0.75, x = 69 + 2, 
                     yend = 22 + 0.75, xend = 69 + 6), color= 'red') + 
    geom_segment(aes(y = 22 - 0.75, x = 69 - 2, 
                     yend = 22 - 0.75, xend = 69 - 6), color = 'red') + 
    geom_segment(aes(y = -22 + 0.75, x = 69 - 2, 
                     yend = -22 + 0.75, xend = 69 - 6), color= 'red') + 
    geom_segment(aes(y = -22 + 0.75, x = 69 + 2, 
                     yend = -22 + 0.75, xend = 69 + 6), color= 'red') + 
    geom_segment(aes(y = -22 - 0.75, x = 69 - 2, 
                     yend = -22 - 0.75, xend = 69 - 6), color = 'red') + 
    geom_segment(aes(y = -22 - 0.75, x = 69 + 2, 
                     yend = -22 - 0.75, xend = 69 + 6), color = 'red') + 
    geom_segment(aes(y = 22 - 0.75, x = 69 + 2, 
                     yend = 22 - 0.75, xend = 69 + 6), color = 'red') + 
    geom_segment(aes(y = 22 + 0.75, x = -69 - 2, 
                     yend = 22 + 0.75, xend = -69 - 6), color = 'red') + 
    geom_segment(aes(y = 22 - 0.75, x = -69 - 2, 
                     yend = 22 - 0.75, xend = -69 - 6), color = 'red') + 
    geom_segment(aes(y = 22 + 0.75, x = -69 + 2, 
                     yend = 22 + 0.75, xend = -69 + 6), color = 'red') + 
    geom_segment(aes(y = -22 + 0.75, x = -69 - 2, 
                     yend = -22 + 0.75, xend = -69 - 6), color = 'red') + 
    geom_segment(aes(y = 22 - 0.75, x = -69 + 2, 
                     yend = 22 - 0.75, xend = -69 + 6), color = 'red') + 
    geom_segment(aes(y = -22 + 0.75, x = -69 + 2, 
                     yend = -22 + 0.75, xend = -69 + 6), color= 'red') + 
    geom_segment(aes(y = -22 - 0.75, x = -69 - 2, 
                     yend = -22 - 0.75, xend = -69 - 6), color = 'red') + 
    geom_segment(aes(y = -22 - 0.75, x = -69 + 2, 
                     yend = -22 - 0.75, xend = -69 + 6), color = 'red') + 
    geom_segment(aes(y = 22 - 15, x = 69 - dd, 
                     yend = 22 - 17, xend = 69 - dd), color = 'red') + 
    geom_segment(aes(y = 22 - 15, x = 69 + dd, 
                     yend = 22 - 17, xend = 69 + dd), color = 'red') + 
    geom_segment(aes(y = 22 + 15, x = 69 + dd, 
                     yend = 22+17, xend = 69 + dd), color = 'red') + 
    geom_segment(aes(y = 22 + 15, x = 69 - dd, 
                     yend = 22 + 17, xend = 69 - dd), color = 'red') + 
    geom_segment(aes(y = -22 + 15, x = 69 - dd, 
                     yend = -22 + 17, xend = 69 - dd), color = 'red') + 
    geom_segment(aes(y = -22 + 15, x = 69 + dd, 
                     yend = -22 + 17, xend = 69 + dd), color = 'red') + 
    geom_segment(aes(y = -22 - 15, x = 69 - dd, 
                     yend = -22 - 17, xend = 69 - dd), color= 'red') + 
    geom_segment(aes(y = -22 - 15, x = 69 + dd, 
                     yend = -22 - 17, xend = 69 + dd), color = 'red') + 
    geom_segment(aes(y = -22 + 15, x = -69 + dd, 
                     yend = -22 + 17, xend = -69 + dd), color = 'red') + 
    geom_segment(aes(y = -22 - 15, x = -69 - dd, 
                     yend = -22 - 17, xend = -69 - dd), color = 'red') + 
    geom_segment(aes(y = -22 - 15, x = -69 + dd, 
                     yend = -22 - 17, xend = -69 + dd), color = 'red') + 
    geom_segment(aes(y = -22 + 15, x = -69 - dd, 
                     yend = -22 + 17, xend = -69 - dd), color = 'red') + 
    geom_segment(aes(y = 22 - 15, x = -69 + dd, 
                     yend = 22 - 17, xend = -69 + dd), color = 'red') + 
    geom_segment(aes(y = 22 - 15, x = -69 - dd, 
                     yend = 22 - 17, xend = -69 - dd), color = 'red') + 
    geom_segment(aes(y = 22 + 15, x = -69 - dd, 
                     yend = 22 + 17, xend = -69 - dd), color = 'red') + 
    geom_segment(aes(y = 22 + 15, x = -69 + dd, 
                     yend = 22 + 17, xend = -69 + dd), color = 'red') + 
    geom_segment(aes(y = 22 + 0.75, x = 69 + 2, 
                     yend = 22 + 3.75, xend = 69 + 2), color = 'red') + 
    geom_segment(aes(y = 22 + 0.75, x = 69 - 2, 
                     yend = 22 + 3.75, xend = 69 - 2), color = 'red') + 
    geom_segment(aes(y = 22 - 0.75, x = 69 + 2, 
                     yend = 22 - 3.75, xend = 69 + 2), color = 'red') + 
    geom_segment(aes(y = 22 - 0.75, x = 69 - 2, 
                     yend = 22 - 3.75, xend = 69 - 2), color = 'red') + 
    geom_segment(aes(y = 22 + 0.75, x = -69 + 2, 
                     yend = 22 + 3.75, xend = -69 + 2), color = 'red') + 
    geom_segment(aes(y = 22 + 0.75, x = -69 - 2, 
                     yend = 22 + 3.75, xend = -69 - 2), color = 'red') + 
    geom_segment(aes(y = 22 - 0.75, x = -69 + 2, 
                     yend = 22 - 3.75, xend = -69 + 2), color = 'red') + 
    geom_segment(aes(y = 22 - 0.75, x = -69 - 2, 
                     yend = 22 - 3.75, xend = -69 - 2), color = 'red') + 
    geom_segment(aes(y = -22 - 0.75, x = -69 + 2, 
                     yend = -22 - 3.75, xend = -69 + 2), color = 'red') + 
    geom_segment(aes(y = -22 - 0.75, x = -69 - 2, 
                     yend = -22 - 3.75, xend = -69 - 2), color = 'red') + 
    geom_segment(aes(y = -22 + 0.75, x = -69 + 2, 
                     yend = -22 + 3.75, xend = -69 + 2), color = 'red') + 
    geom_segment(aes(y = -22 + 0.75, x = -69 - 2, 
                     yend = -22 + 3.75, xend = -69 - 2), color = 'red') + 
    geom_segment(aes(y = -22 + 0.75, x = 69 + 2, 
                     yend = -22 + 3.75, xend = 69 + 2), color = 'red') + 
    geom_segment(aes(y = -22 - 0.75, x = 69 - 2, 
                     yend = -22 - 3.75, xend = 69 - 2), color = 'red') + 
    geom_segment(aes(y = -22 + 0.75, x = 69 - 2, 
                     yend = -22 + 3.75, xend = 69 - 2), color = 'red') + 
    geom_segment(aes(y = -22 - 0.75, x = 69 + 2, 
                     yend = -22 - 3.75, xend = 69 + 2), color = 'red') + 
    geom_path(data = data.frame(y = 22 + 15 * cos(theta), 
                                x = 69 + 15 * sin(theta)), color = 'red') + 
    geom_path(data = data.frame(y = 22 + 15 * cos(theta), 
                                x = -69 + 15 * sin(theta)), color = 'red') + 
    geom_path(data = data.frame(y = -22 + 15 * cos(theta), 
                                x = -69 + 15 * sin(theta)), color = 'red') + 
    geom_path(data = data.frame(y = -22 + 15 * cos(theta), 
                                x = 69 + 15 * sin(theta)), color = 'red') + 
        
    theme_void()
}

rink <- fun.draw_rink() + coord_fixed()


     