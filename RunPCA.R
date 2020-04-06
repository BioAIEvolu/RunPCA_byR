#install.packages(c("ggplot2","pca3d","rgl"))
library(ggplot2)
library(pca3d)
library(rgl)

setwd('D:\\11810\\Documents\\RunPCA')

#读取矩阵
data<-read.table('input.txt', header=T, sep='\t', row.names=1)
#View(data)

#矩阵转置
data<-t(as.matrix(data))
data.class<-rownames(data)

# 运行PCA
data.pca<-prcomp(data, scale. = TRUE)

#输出结果文件
write.table(data.pca$rotation, file='PC.xls', quote=F, sep='\t') #输出特征向量
write.table(predict(data.pca), file='newTab.xls', quote = F, sep='\t') #输出新表

pca.sum=summary(data.pca)
write.table(pca.sum$importance, file='importance.xls', quote=F, sep='\t') #输出PC比重


#绘图
pdf(file='pcaBarplot.pdf', width = 15)
barplot(pca.sum$importance[2, ]*100, xlab='PC', ylab='percent', col='skyblue')  #柱状图
dev.off()

pdf(file='pcaElbowPlot.pdf', width=15)
plot(pca.sum$importance[2, ]*100, type='o', col='red', xlab='PC', ylab='percent')  #碎石图
dev.off()

#pca 2d plot
library(ggplot2)
group=c(rep('con', 5), rep('A', 5), rep('B', 3))
pcaPredict<-predict(data.pca)
PCA<-data.frame(PCA1=pcaPredict[, 1], PCA2=pcaPredict[, 2], group=group)
PCA.mean<-aggregate(PCA[,1:2], by=list(group=PCA$group), FUN=mean)

#定义椭圆
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}
df_ell <- data.frame()
for(g in levels(PCA$group)){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(PCA[PCA$group==g,],
                                veganCovEllipse(cov.wt(cbind(PCA1,PCA2),
                                wt=rep(1/length(PCA1),length(PCA1)))$cov,
                                center=c(mean(PCA1),mean(PCA2))))),group=g))
}

pdf(file="PCA2d.pdf")
ggplot(data = PCA, aes(PCA1, PCA2)) + geom_point(aes(color = group)) +
  geom_path(data=df_ell, aes(x=PCA1, y=PCA2, colour=group), size=1, linetype=2)+
  annotate("text",x=PCA.mean$PCA1,y=PCA.mean$PCA2,label=PCA.mean$group)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dev.off()

#pca 3d plot
library(pca3d)
library(rgl)

pca3d(data.pca, components = 1:3, group = group, 
      show.centroids = TRUE, show.group.labels = TRUE) #绘制3d图
rgl.snapshot('pca3d.png', fmt='png')
