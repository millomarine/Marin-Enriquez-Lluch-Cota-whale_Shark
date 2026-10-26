
### frunción para derivadas ####
deriv1d <- function(x, h){
  
  n <- length(x)
  d <- rep(NA_real_, n)
  
  for(i in seq_len(n)){
    
    # Diferencia central
    if(i > 1 && i < n){
      
      if(!is.na(x[i-1]) && !is.na(x[i+1])){
        
        d[i] <- (x[i+1]-x[i-1])/(2*h)
        next
        
      }
      
    }
    
    # Diferencia hacia adelante
    if(i < n){
      
      if(!is.na(x[i]) && !is.na(x[i+1])){
        
        d[i] <- (x[i+1]-x[i])/h
        next
        
      }
      
    }
    
    # Diferencia hacia atrás
    if(i > 1){
      
      if(!is.na(x[i]) && !is.na(x[i-1])){
        
        d[i] <- (x[i]-x[i-1])/h
        
      }
      
    }
    
  }
  
  d
}
lon <- longi
lat <- lati
d_dx <- function(F, lon, lat){
  
  R <- 6371000
  
  nx <- nrow(F)
  ny <- ncol(F)
  
  dlon <- mean(diff(lon))*pi/180
  
  out <- matrix(NA_real_, nx, ny)
  
  for(j in 1:ny){
    
    dx <- R*cos(lat[j]*pi/180)*dlon
    
    for(i in 2:(nx-1)){
      
      if(!is.na(F[i-1,j]) && !is.na(F[i+1,j])){
        
        out[i,j] <- (F[i+1,j]-F[i-1,j])/(2*dx)
        
      } else if(!is.na(F[i,j]) && !is.na(F[i+1,j])){
        
        out[i,j] <- (F[i+1,j]-F[i,j])/dx
        
      } else if(!is.na(F[i,j]) && !is.na(F[i-1,j])){
        
        out[i,j] <- (F[i,j]-F[i-1,j])/dx
        
      }
      
    }
    
  }
  
  out
}
d_dy <- function(F, lon, lat){
  
  R <- 6371000
  
  nx <- nrow(F)
  ny <- ncol(F)
  
  dlat <- mean(diff(lat))*pi/180
  dy <- R*dlat
  
  out <- matrix(NA_real_, nx, ny)
  
  for(i in 1:nx){
    
    for(j in 2:(ny-1)){
      
      if(!is.na(F[i,j-1]) && !is.na(F[i,j+1])){
        
        out[i,j] <- (F[i,j+1]-F[i,j-1])/(2*dy)
        
      } else if(!is.na(F[i,j]) && !is.na(F[i,j+1])){
        
        out[i,j] <- (F[i,j+1]-F[i,j])/dy
        
      } else if(!is.na(F[i,j]) && !is.na(F[i,j-1])){
        
        out[i,j] <- (F[i,j]-F[i,j-1])/dy
        
      }
      
    }
    
  }
  
  out
}

