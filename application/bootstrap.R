source("bGEV_loss_functions.R")

args=commandArgs(trailingOnly=T)
print(args)
boot.num=as.numeric(args[1])


reticulate::use_virtualenv("myenv", required = T)
library(keras)
sess = k_get_session()
sess$list_devices()
library(tensorflow)
tf$random$set_seed(3)



nunits.xi<-eval(parse(text=args[2]))
nunits.q<-eval(parse(text=args[3]))
nunits.s<-eval(parse(text=args[4]))
load("Data/monthly_max_data.Rdata")



#Normalise inputs
for(i in 1:dim(X)[3]){
  temp=X[,,i]
  m=mean( temp,na.rm=T)
  s=sd( temp,na.rm=T)
  temp=( temp-m)/s
  X[,,i]=temp
}


set.seed(boot.num)


N=dim(Y)[1]

all_inds=sample(1:N,N,replace=T)


Y.boot<-Y[all_inds,]
X.boot<-X[c(all_inds),,]


#Shuffle all observations
shuffle.inds=sample(1:length(Y))


Y.boot<-c(Y.boot)
tmp=matrix(nrow=length(Y),ncol=dim(X.boot)[3])
for(i in 1:dim(X)[3]) tmp[,i]=X.boot[,,i]
X.boot<-tmp


#Subset into validation and training data
#Make 20% validation data
valid.inds=sample(1:nrow(X.boot),nrow(X.boot)/10)
test.inds=sample((1:nrow(X.boot))[-valid.inds],nrow(X.boot)/10)
Y.train <- Y.boot[-valid.inds]; X.train <- X.boot[-valid.inds,]
Y.valid <- Y.boot[valid.inds]; X.valid <- X.boot[valid.inds,]
Y.test <- Y.boot[valid.inds]; X.test <- X.boot[valid.inds,]
Y.train <- as.matrix(Y.train); Y.valid <- as.matrix(Y.valid); Y.test <- as.matrix(Y.test)

# Build model - MLP for sigma and mu, constant for xi
input_nn <- layer_input(shape = dim(X.boot)[2], name = 'nn_input')

init_xi=0.1
init_loc = 3
init_spread = 1

xiBranch <- input_nn %>%
  layer_dense(units = nunits.xi[1], activation = 'relu',
              input_shape =dim(X.boot)[2], name = 'nonlin_xi_dense1', kernel_regularizer = regularizer_l1_l2(l1 = 1e-4, l2 = 1e-4)) %>%
  layer_dropout(0.3)
for(i in 2:length(nunits.xi)){ 
  xiBranch <- xiBranch %>%
    layer_dense(units=nunits.xi[i],activation = 'relu', name = paste0("nonlin_xi_dense",i), 
                kernel_regularizer = regularizer_l1_l2(l1 = 1e-4, l2 = 1e-4)) %>%
    layer_dropout(0.3)
}    
xiBranch <- xiBranch %>% layer_dense(units = 1, activation = "sigmoid" , name = paste0("nonlin_xi_dense"),
                                     weights=list(matrix(0,nrow=nunits.xi[length(nunits.xi)],ncol=1), array(qlogis(init_xi/0.25))))

qBranch <- input_nn %>%
  layer_dense(units = nunits.q[1], activation = 'relu',
              input_shape =dim(X.boot)[2], name = 'nonlin_q_dense1', kernel_regularizer = regularizer_l1_l2(l1 = 1e-4, l2 = 1e-4)) %>%
  layer_dropout(0.3)
for(i in 2:length(nunits.q)){ 
  qBranch <- qBranch %>%
    layer_dense(units=nunits.q[i],activation = 'relu', name = paste0("nonlin_q_dense",i), 
                kernel_regularizer = regularizer_l1_l2(l1 = 1e-4, l2 = 1e-4)) %>%
    layer_dropout(0.3)
}    
qBranch <- qBranch %>% layer_dense(units = 1, activation = "exponential", name = paste0("nonlin_q_dense"),
                                   weights=list(matrix(0,nrow=nunits.q[length(nunits.q)],ncol=1), array(log(init_loc))))

sBranch <- input_nn %>%
  layer_dense(units = nunits.s[1], activation = 'relu',
              input_shape =dim(X.boot)[2], name = 'nonlin_s_dense1', kernel_regularizer = regularizer_l1_l2(l1 = 1e-4, l2 = 1e-4)) %>%
  layer_dropout(0.3)
for(i in 2:length(nunits.s)){ 
  sBranch <- sBranch %>%
    layer_dense(units=nunits.s[i],activation = 'relu', name = paste0("nonlin_s_dense",i), 
                kernel_regularizer = regularizer_l1_l2(l1 = 1e-4, l2 = 1e-4)) %>%
    layer_dropout(0.3)
}    
sBranch <- sBranch %>% layer_dense(units = 1, activation = "exponential", name = paste0("nonlin_s_dense"),
                                   weights=list(matrix(0,nrow=nunits.s[length(nunits.s)],ncol=1), array(log(init_spread))))



output <- layer_concatenate(c(qBranch,sBranch, xiBranch))

model <- keras_model(
  inputs = c(input_nn), 
  outputs = c(output)
)
summary(model)


loss<-bGEV_loss()

model %>% compile(
  optimizer="adam",
  loss = loss,
  run_eagerly=T
)

checkpoint <- callback_model_checkpoint(paste0("Models/application/boot_",boot.num), monitor = "val_loss", verbose = 0,
                                        save_best_only = TRUE, save_weights_only = TRUE, mode = "min",
                                        save_freq = "epoch")



history <- model %>% fit(
  list(X.train), Y.train, shuffle=T,
  epochs = 250, batch_size = 1024,
  callback=list(checkpoint,callback_early_stopping(
    monitor = "val_loss", min_delta = 0, patience = 5)),
  validation_data=list(list( nn_input=X.valid),Y.valid),
  verbose=2
)

model <- load_model_weights_tf(model,filepath=paste0("Models/application/boot_",boot.num))


preds.test<-model %>% predict(X.test)
print(paste0("Test loss = ", 
             round(
               k_get_value(loss(k_constant(Y.test),k_constant(preds.test))),
               digits=3)
)
)

Y.pred<-c(Y)
tmp=matrix(nrow=length(Y),ncol=dim(X)[3])
for(i in 1:dim(X)[3]) tmp[,i]=c(X[,,i])
X.pred<-tmp

preds<-model %>% predict(X.pred)
exp_dat=apply(cbind(Y.pred,preds),1,function(x){
  
  pbGEV(x[1],q_a=x[2],s_b=x[3],xi=x[4],
        alpha=0.5,beta=0.5,p_a=0.05,p_b=0.2,c1=5,c2=5)
  
})

exp_dat=qexp(exp_dat)
p_min=0
n_p=length(exp_dat)*(1-p_min)
ps=p_min+(1:n_p)/(n_p+1)*(1-p_min)
pdf(file=paste0("application/Figures/QQ_plot_boot",boot.num,".pdf"))
plot(quantile(exp_dat,ps),qexp(ps),xlab="Fitted",ylab="Empirical",main="Test")
abline(a=0,b=1,col="red")
dev.off()

dim(preds)=c(nrow(X),ncol(X),3)


save(preds,file=paste0("application/Predictions/preds_boot",boot.num,".Rdata"))

S=cbind(c(coords[,1]),c(coords[,2]))

library(ggplot2)
library(ggmap)

indbox = make_bbox(lon = c(min(S[ , 1]), max(S[ , 1])), lat = c(max(S[ , 2]), min(S[ , 2])), f = 0)

world <- map_data("world")


ggp = ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = "lightgray", size = 0.1
  ) + scale_x_continuous(limits=indbox[c(1,3)], expand = c(0, 0)) + scale_y_continuous(limits=indbox[c(2,4)], expand = c(0, 0)) + coord_fixed(ratio=1.4)


time.ind<-30



temp= preds[time.ind,,1]
cols=c('#fff7ec','#fee8c8','#fdd49e','#fdbb84','#fc8d59','#ef6548','#d7301f','#b30000','#7f0000')
brks=seq(floor(min((temp))),ceiling(max((temp))),length=10)
brks=ceiling(brks*10)/10


data = data.frame(lon = S[ , 1], lat = S[ , 2], value = temp)
p0 = ggp +  theme(axis.line = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  plot.margin = unit(c(0, 0, -1, -1), 'lines'))+geom_tile(data, mapping = aes(x = lon, y = lat, fill = value), width = 0.27, height = 0.27) + xlab("") + ylab("")
#plot(p0)
p1 = p0 +scale_fill_stepsn(breaks=brks,colors=cols,name="",limits=range(brks))+theme(legend.key.size = unit(0.7, "in"),legend.text=element_text(size=15),
                                                                                     plot.background = element_rect(fill = "transparent", colour = NA),
                                                                                     legend.background  = element_rect(fill = "transparent", colour = NA),
)
# plot(p1)


p2 = p1 + geom_path(data=world, aes(x=long, y=lat, group=group), colour="black") 
p2 <- p2 + theme(rect = element_rect(fill = "transparent"))
ggsave(p2, filename =paste0("application/Figures/loc_t",time.ind,"_boot",boot.num,".pdf"),
       height = 3.8, width = 8, bg="transparent")


temp= preds[time.ind,,2]
cols=c('#fff7ec','#fee8c8','#fdd49e','#fdbb84','#fc8d59','#ef6548','#d7301f','#b30000','#7f0000')
brks=seq(floor(min((temp))),ceiling(max((temp))),length=10)
brks=ceiling(brks*10)/10


data = data.frame(lon = S[ , 1], lat = S[ , 2], value = temp)
p0 = ggp +  theme(axis.line = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  plot.margin = unit(c(0, 0, -1, -1), 'lines'))+geom_tile(data, mapping = aes(x = lon, y = lat, fill = value), width = 0.27, height = 0.27) + xlab("") + ylab("")
#plot(p0)
p1 = p0 +scale_fill_stepsn(breaks=brks,colors=cols,name="",limits=range(brks))+theme(legend.key.size = unit(0.7, "in"),legend.text=element_text(size=15),
                                                                                     plot.background = element_rect(fill = "transparent", colour = NA),
                                                                                     legend.background  = element_rect(fill = "transparent", colour = NA),
)
# plot(p1)


p2 = p1 + geom_path(data=world, aes(x=long, y=lat, group=group), colour="black") 
p2 <- p2 + theme(rect = element_rect(fill = "transparent"))
ggsave(p2, filename =paste0("application/Figures/s_t",time.ind,"_boot",boot.num,".pdf"),
       height = 3.8, width = 8, bg="transparent")

temp= preds[time.ind,,3]
cols=c('#fff7ec','#fee8c8','#fdd49e','#fdbb84','#fc8d59','#ef6548','#d7301f','#b30000','#7f0000')
brks=seq((min((temp))),max((temp)),length=10)
brks=ceiling(brks*100)/100


data = data.frame(lon = S[ , 1], lat = S[ , 2], value = temp)
p0 = ggp +  theme(axis.line = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  plot.margin = unit(c(0, 0, -1, -1), 'lines'))+geom_tile(data, mapping = aes(x = lon, y = lat, fill = value), width = 0.27, height = 0.27) + xlab("") + ylab("")
#plot(p0)
p1 = p0 +scale_fill_stepsn(breaks=brks,colors=cols,name="",limits=range(brks))+theme(legend.key.size = unit(0.7, "in"),legend.text=element_text(size=15),
                                                                                     plot.background = element_rect(fill = "transparent", colour = NA),
                                                                                     legend.background  = element_rect(fill = "transparent", colour = NA),
)
# plot(p1)


p2 = p1 + geom_path(data=world, aes(x=long, y=lat, group=group), colour="black") 
p2 <- p2 + theme(rect = element_rect(fill = "transparent"))
ggsave(p2, filename =paste0("application/Figures/xi_t",time.ind,"_boot",boot.num,".pdf"),
       height = 3.8, width = 8, bg="transparent")



temp= apply(preds[time.ind,,],1,function(x){
  
  qbGEV(0.9,q_a=x[1],s_b=x[2],xi=x[3],
        alpha=0.5,beta=0.5,p_a=0.05,p_b=0.2,c1=5,c2=5)
  
})

cols=c('#fff7ec','#fee8c8','#fdd49e','#fdbb84','#fc8d59','#ef6548','#d7301f','#b30000','#7f0000')
brks=seq((min((temp))),max((temp)),length=10)
brks=ceiling(brks*10)/10


data = data.frame(lon = S[ , 1], lat = S[ , 2], value = temp)
p0 = ggp +  theme(axis.line = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  plot.margin = unit(c(0, 0, -1, -1), 'lines'))+geom_tile(data, mapping = aes(x = lon, y = lat, fill = value), width = 0.27, height = 0.27) + xlab("") + ylab("")
#plot(p0)
p1 = p0 +scale_fill_stepsn(breaks=brks,colors=cols,name="",limits=range(brks))+theme(legend.key.size = unit(0.7, "in"),legend.text=element_text(size=15),
                                                                                     plot.background = element_rect(fill = "transparent", colour = NA),
                                                                                     legend.background  = element_rect(fill = "transparent", colour = NA),
)
# plot(p1)


p2 = p1 + geom_path(data=world, aes(x=long, y=lat, group=group), colour="black") 
p2 <- p2 + theme(rect = element_rect(fill = "transparent"))
ggsave(p2, filename =paste0("application/Figures/quant_t",time.ind,"_boot",boot.num,".pdf"),
       height = 3.8, width = 8, bg="transparent")