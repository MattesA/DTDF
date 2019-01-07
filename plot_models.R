

# Check if necessary packages are installed and install missing packages
ins <- rownames(installed.packages())  # installed packages
rqd <- c("magick", "ggplot2")  # required packages
pkg <- rqd[!rqd %in% ins]  # packages that need to be installed
if (length(pkg) > 0) install.packages(pkg)  # install them


library(magick)
library(ggplot2)

plot.models <- function(soa, rsp, rarsp, p, mr, rs, 
                        models = c("sequential", "parallel", "crosstalk", "integrated"), ...) {
  
  # rsp: response selection proportion
  # rarsp: response activation/response selection proportion
  
  results <- list()
  
  # Configurations
  h <- 100
  
  # Custom configurations
  ra <- rarsp*rs
  
  
  # Do not change
  rs.area <- rs*h
  rs1 <- rs.area/(h*rsp)
  rs2 <- rs.area/(h*(1-rsp))
  rs1a <- min((soa+p) - (p), rs)
  rs1b.area <- rs.area - rs1a*h
  rs1b <- rs1b.area/(h*rsp)
  rs2a <- rs1b
  rs2b.area <- rs.area - rs2a*h*(1-rsp)
  rs2b <- rs2b.area/(h)
  
  ra.area <- ra*h
  ra1 <- ra.area/(h*rsp)
  ra2 <- ra.area/(h*(1-rsp))
  ra1a <- min((soa + p) - (p), ra)
  ra1b.area <- ra.area - ra1a*h
  ra1b <- ra1b.area/(h*rsp)
  ra2a <- ra1b
  ra2b.area <- ra.area - ra2a*h*(1-rsp)
  ra2b <- ra2b.area/h
  
  t <- max(soa+p+2*rs+mr, 
           soa+p+rs2a+rs2b+mr,
           soa+p+rs+rarsp*rs+mr,
           soa+p+ra2a+ra2b+(1-rarsp)*rs*2+mr)
  
  
  # Do not change
  background <- image_blank(width = t, height = h*2.5, color = "white")
  
  perception1 <- image_blank(width = p-0.02*h, height = h*0.98, color = "white") %>%
    image_border(color = "black", geometry = paste(0.01*h, "x", 0.01*h)) %>%
    image_annotate(text = "P1", gravity = "center", size = 0.2*h)
  perception2 <- image_blank(width = p-0.02*h, height = h*0.98, color = "white") %>%
    image_border(color = "black", geometry = paste(0.01*h, "x", 0.01*h)) %>%
    image_annotate(text = "P2", gravity = "center", size = 0.2*h)
  
  response1 <- image_blank(width = mr-0.02*h, height = h*0.98, color = "white") %>%
    image_border(color = "black", geometry = paste(0.01*h, "x", 0.01*h)) %>%
    image_annotate(text = "MR1", gravity = "center", size = 0.2*h)
  response2 <- image_blank(width = mr-0.02*h, height = h*0.98, color = "white") %>%
    image_border(color = "black", geometry = paste(0.01*h, "x", 0.01*h)) %>%
    image_annotate(text = "MR2", gravity = "center", size = 0.2*h)
  
  respact1 <- image_blank(width = ra-0.02*h, height = h*0.98, color = "white") %>%
    image_border(color = "black", geometry = paste(0.01*h, "x", 0.01*h)) %>%
    image_annotate(text = "RA1", gravity = "center", size = 0.2*h)
  respact1a <- image_blank(width = ra1a, height = h, color = "lightgray") 
  respact1b <- image_blank(width = ra1b, height = h*rsp, color = "lightgray") %>%
    image_annotate(text = "RA1", gravity = "center", size = 0.2*h)
  
  respact2 <- image_blank(width = ra-0.02*h, height = h*0.98, color = "white") %>%
    image_border(color = "black", geometry = paste(0.01*h, "x", 0.01*h)) %>%
    image_annotate(text = "RA2", gravity = "center", size = 0.2*h)
  respact2a <- image_blank(width = ra2a, height = h*(1-rsp), color = "lightgray") %>%
    image_annotate(text = "RA2", gravity = "center", size = 0.2*h)
  respact2b <- image_blank(width = ra2b, height = h, color = "lightgray") %>%
    image_annotate(text = "RA2", gravity = "center", size = 0.2*h)
  
  respsel1 <- image_blank(width = rs-0.02*h, height = h*0.98, color = "gray") %>%
    image_border(color = "black", geometry = paste(0.01*h, "x", 0.01*h)) %>%
    image_annotate(text = "RS1", gravity = "center", size = 0.2*h)
  respsel1a <- image_blank(width = rs1a, height = h, color = "lightgray") 
  respsel1b <- image_blank(width = rs1b, height = h*rsp, color = "lightgray") %>%
    image_annotate(text = "RS1", gravity = "center", size = 0.2*h)
  
  respsel2 <- image_blank(width = rs-0.02*h, height = h*0.98, color = "gray") %>%
    image_border(color = "black", geometry = paste(0.01*h, "x", 0.01*h)) %>%
    image_annotate(text = "RS2", gravity = "center", size = 0.2*h)
  respsel2a <- image_blank(width = rs2a, height = h*(1-rsp), color = "lightgray") %>%
    image_annotate(text = "RS2", gravity = "center", size = 0.2*h)
  respsel2b <- image_blank(width = rs2b, height = h, color = "lightgray")
  
  timeline <- image_blank(width = t, height = 0.01*h, color = "white")
  
  
  
  # Compute and plot the models
  if ("sequential" %in% models) {
    
    sequential <- image_composite(image = background, composite_image = timeline, offset = paste("+", 0, "+", 0.99*2.5*h)) %>%
      # Task 1
      image_composite(composite_image = perception1, offset = paste("+", 0, "+", 0)) %>%  # P1
      image_composite(composite_image = respsel1, offset = paste("+", p, "+", 0)) %>%
      image_composite(composite_image = response1, offset = paste("+", p+rs, "+", 0)) %>%
      # Task 2
      image_composite(composite_image = perception2, offset = paste("+", soa, "+", 0.57*2.5*h)) %>%  # P1
      image_composite(composite_image = respsel2, offset = paste("+", max(p+rs, soa+p), "+", 0.57*2.5*h)) %>%
      image_composite(composite_image = response2, offset = paste("+", max(p+rs, soa+p)+rs, "+", 0.57*2.5*h))
    
    #results$sequential <- as.raster(sequential)
    results$sequential <- image_ggplot(sequential)
    
  }
  
  if ("parallel" %in% models) {
    
    if (rs1b == 0 & rs2a == 0) {  # if response selection doesn't overlap...
      
      # ... write the labels into the geometric figures which are displayed (i.e. rs1a and rs2b)
      respsel1a <- image_blank(width = rs1a, height = h, color = "lightgray") %>%
        image_annotate(text = "RS1", gravity = "center", size = 0.2*h)
      respsel2b <- image_blank(width = rs2b, height = h, color = "lightgray") %>%
        image_annotate(text = "RS2", gravity = "center", size = 0.2*h)
      
    }
    
    parallel <- image_composite(image = background, composite_image = timeline, offset = paste("+", 0, "+", 0.99*5*h)) %>%
      # Task 1
      image_composite(composite_image = perception1, offset = paste("+", 0, "+", 0)) %>%  # P1
      image_composite(composite_image = respsel1a, offset = paste("+", p, "+", 0)) %>%
      image_composite(composite_image = respsel1b, offset = paste("+", p+rs1a, "+", 0)) %>%
      image_composite(composite_image = response1, offset = paste("+", p+rs1a+rs1b, "+", 0)) %>%
      # Task 2
      image_composite(composite_image = perception2, offset = paste("+", soa, "+", 0.57*2.5*h)) %>%  # P1
      image_composite(composite_image = respsel2a, offset = paste("+", soa+p, "+", 0.57*2.5*h+rsp*h)) %>%
      image_composite(composite_image = respsel2b, offset = paste("+", soa+p+rs2a, "+", 0.57*2.5*h)) %>%
      image_composite(composite_image = response2, offset = paste("+", soa+p+rs2a+rs2b, "+", 0.57*2.5*h))
    
    #results$parallel <- as.raster(parallel)
    results$parallel <- image_ggplot(parallel)
    
  }
  
  
  # The following two models split up the response selection process into response activation and response selection
  rs <- rs - ra
  
  # Compute new RS values
  rs.area <- rs*h
  rs1 <- rs.area/(h*rsp)
  rs2 <- rs.area/(h*(1-rsp))
  rs1a <- min((soa+p) - (p), rs)
  rs1b.area <- rs.area - rs1a*h
  rs1b <- rs1b.area/(h*rsp)
  rs2a <- rs1b
  rs2b.area <- rs.area - rs2a*h*(1-rsp)
  rs2b <- rs2b.area/(h)
  
  respsel1 <- image_blank(width = rs-0.02*h, height = h*0.98, color = "gray") %>%
    image_border(color = "black", geometry = paste(0.01*h, "x", 0.01*h)) %>%
    image_annotate(text = "RS1", gravity = "center", size = 0.2*h)
  respsel1a <- image_blank(width = rs1a, height = h, color = "lightgray") 
  respsel1b <- image_blank(width = rs1b, height = h*rsp, color = "lightgray") %>%
    image_annotate(text = "RS1", gravity = "center", size = 0.2*h)
  
  respsel2 <- image_blank(width = rs-0.02*h, height = h*0.98, color = "gray") %>%
    image_border(color = "black", geometry = paste(0.01*h, "x", 0.01*h)) %>%
    image_annotate(text = "RS2", gravity = "center", size = 0.2*h)
  respsel2a <- image_blank(width = rs2a, height = h*(1-rsp), color = "lightgray") %>%
    image_annotate(text = "RS2", gravity = "center", size = 0.2*h)
  respsel2b <- image_blank(width = rs2b, height = h, color = "lightgray")
  
  
  if ("crosstalk" %in% models) {
    
    # Plot the model
    crosstalk <- image_composite(image = background, composite_image = timeline, offset = paste("+", 0, "+", 0.99*5*h)) %>%
      # Task 1
      image_composite(composite_image = perception1, offset = paste("+", 0, "+", 0)) %>%  # P1
      image_composite(composite_image = respact1, offset = paste("+", p, "+", 0)) %>%
      image_composite(composite_image = respsel1, offset = paste("+", p+ra, "+", 0)) %>%
      image_composite(composite_image = response1, offset = paste("+", p+ra+rs, "+", 0)) %>%
      # Task 2
      image_composite(composite_image = perception2, offset = paste("+", soa, "+", 0.57*2.5*h)) %>%  # P1
      image_composite(composite_image = respact2, offset = paste("+", soa+p, "+", 0.57*2.5*h)) %>%
      image_composite(composite_image = respsel2, offset = paste("+", max(p+ra+rs, soa+p+ra), "+", 0.57*2.5*h)) %>%
      image_composite(composite_image = response2, offset = paste("+", max(p+ra+rs, soa+p+ra)+rs, "+", 0.57*2.5*h))
  
    #results$crosstalk <- as.raster(crosstalk)
    results$crosstalk <- image_ggplot(crosstalk)
    
  }
  
  if ("integrated" %in% models) {
    
    if (ra1b == 0 & ra2a == 0) {  # same as with parallel --> overlapping response activation
      
      respact1a <- image_blank(width = ra1a, height = h, color = "lightgray") %>%
        image_annotate(text = "RA1", gravity = "center", size = 0.2*h)
      
    }
    
    # Plot the model
    integrated <- image_composite(image = background, composite_image = timeline, offset = paste("+", 0, "+", 0.99*5*h)) %>%
      # Task 1
      image_composite(composite_image = perception1, offset = paste("+", 0, "+", 0)) %>%  # P1
      image_composite(composite_image = respact1a, offset = paste("+", p, "+", 0)) %>%
      image_composite(composite_image = respact1b, offset = paste("+", p+ra1a, "+", 0)) %>%
      image_composite(composite_image = respsel1, offset = paste("+", p+ra1a+ra1b, "+", 0)) %>%
      image_composite(composite_image = response1, offset = paste("+", p+ra1a+ra1b+rs, "+", 0)) %>%
      # Task 2
      image_composite(composite_image = perception2, offset = paste("+", soa, "+", 0.57*2.5*h)) %>%  # P1
      image_composite(composite_image = respact2a, offset = paste("+", soa+p, "+", 0.57*2.5*h+rsp*h)) %>%
      image_composite(composite_image = respact2b, offset = paste("+", max(p+ra1a+ra1b+rs, soa+p), "+", 0.57*2.5*h)) %>%
      image_composite(composite_image = respsel2, offset = paste("+", max(p+ra1a+ra1b+rs, soa+p)+ra2b, "+", 0.57*2.5*h)) %>%
      image_composite(composite_image = response2, offset = paste("+", max(p+ra1a+ra1b+rs, soa+p)+ra2b+rs, "+", 0.57*2.5*h))
    
    #results$integrated <- as.raster(integrated)
    results$integrated <- image_ggplot(integrated)
    
  }
  
  return(results)
  
}