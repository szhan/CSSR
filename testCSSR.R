library(CSSR)
library(DOT)

test <- runCSSR(alphabet="01",data="0101010101010101010101",maxLength=3,isChi=FALSE,sigLevel=0.001,outputPrefix="")
test$c_mu

#Non stationary#
pGen <- function(t,z){(0.5*t^3)/(t^3+z^3)}
t <- 1:2000
p_coin <- pGen(t,500)
plot(p_coin,xlab="time",ylab="p")
abline(v=1000)

x <- rbinom(length(p_coin),1,p_coin)
x <- paste(x,collapse="")
test <- runCSSR(alphabet="01",data=x,maxLength=3,isChi=FALSE,sigLevel=0.001,outputPrefix="")

varCmu <- function(sequence,w){
  print(nchar(sequence)-w+1)

  cmu_vec <- numeric(nchar(sequence)-w+1)
  hmu_vec <-numeric(nchar(sequence)-w+1)

  end = (nchar(sequence)-w+1)
  for(i in 1:end){
    print(paste0("iter: ",i," of ",end))
    out <- runCSSR(alphabet="01",data=substr(sequence,i,i+w-1),maxLength=3,isChi=FALSE,sigLevel=0.001,outputPrefix="")
    cmu_vec[i] <- out$c_mu
    hmu_vec[i] <- out$ent_rate
    gc()
  }
  return(c(var(cmu_vec),var(hmu_vec)))
}

# setwd("/home/jake/CSSS/Memory/wsweep")
a <- varCmu(x,1000)
print(a)


# run it in parallel
library(parallel)
w = 1000

sequence <- rbinom(length(p_coin),1,p_coin)
sequence <- paste(sequence,collapse="")

cmu_vec <- numeric(nchar(sequence)-w+1)
hmu_vec <-numeric(nchar(sequence)-w+1)
end = (nchar(sequence)-w+1)

varCmu = parallel::mclapply(X = 1:end,FUN = function(i,sequence,w){
  invisible(out <- runCSSR(alphabet="01",data=substr(sequence,i,i+w-1),maxLength=3,isChi=FALSE,sigLevel=0.001,outputPrefix=""))
  # cmu_vec[i] <- out$c_mu
  # hmu_vec[i] <- out$ent_rate
  return(list(
    c_mu = out$c_mu,
    ent_rate = out$ent_rate
  ))
},sequence=sequence,w=w)
