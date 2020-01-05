feature<-read.csv(file="F:\\SDMV\\vaccine\\dfeature.csv",sep=",",header = FALSE)
print(feature)
feature[1,]
head(feature)
class<-read.csv(file = "F:\\SDMV\\vaccine\\class_seq.csv",sep=",",header = FALSE)
print(class)
#feature[163,1]
#r=sample(1:220,220,replace=FALSE)
class(feature)
random=sample(1:nrow(feature),nrow(feature))
print (random)
is.vector(random)

#TRAINING SET
feature_train=feature[random[1:176],]
print(feature_train)
feature_train[2,24]

#TEST SET
feature_test=feature[random[177:220],]
print(feature_test)

nrow(feature_train)
nrow(feature_test)
a=1
s=0
distance=c()



feature_train[,1]
feature_test[44,1]
#warnings(50)
print(distance)
seqn<-vector(mode="list",length=nrow(feature_train))
names(seqname)<-(feature_train[,1])

seqntest<-vector(mode="list",length=nrow(feature_test))
names(seqntest)<-(feature_test[,1])
seqntest[44]
seqname[7]

#DISTANCE BASED PREDICTION
testse=c()
lesname=c()

for (i in c(1:nrow(feature_test)))
{
  test1<-feature_test[i,2:23]
  for (a in c(1:nrow(feature_train)))
  {
    
    train1<-feature_train[a,2:23]
    differ<-test1-train1
    d=((differ)**2)
    s=sum(d)
    dist=sqrt(s)
    names(dist)<-feature_train[a,24]
    distance<-append(distance,dist)
    #seqt<-append(seqn,dist)
    #n<-append(n,feature_train[a,1])
    #names(seqt)<-n
  }
  #print(feature_test[i,1])
  #seqt<-append(seqn,dist)
  #n<-append(n,feature_test[i,1])
  #names(seqt)<-n
  testse<-append(testse,i)
  lesname<-append(lesname,feature_test[i,24])
  #names(a)<-feature_test
}
names(testse)<-lesname
testse[1]
lesname
#seqt[distance[1]]
distance[2]
print (distance)
#seqn
class(distance)
multiple=c()
for (m in (1:44))
     {
       multi=176*m
       multiple<-append(multiple,multi)
       
       
}
print(multiple)

#seqtrain=c()
#na=c()
#na
q=1
sortdist=c()
sortd=c()
mat<-matrix()
poscount=0
negcount=0
prediction=c()
for (n in multiple)
{
  
  sortdist<-sort(distance[q:n],decreasing = FALSE)
  q=n+1
  pred<-names(sortdist[1:5]) #k-5
  print (pred)
  #mat_sort<-rbind(mat,sortdist)
  for (w in pred)
  {
    if (w=="1")
    {
      poscount=poscount + 1
      #print(poscount)
      
    }
    else
    {
      negcount=negcount + 1
      #print(negcount)
    }
  }
 if (poscount>negcount)
    {
      p=1
      prediction<-append(prediction,p) 
      
    }
 else
    {
      
      p=-1
      prediction<-append(prediction,p)
    }
  
poscount=0
negcount=0


  #sortd<-c(sortd,sortdist)
}
#finding true positives,false positives,true negatives, false negatives
print (prediction)
print(lesname)
tp=0
tn=0
fp=0
fn=0
for (nm in c(1:44))
{
  if(lesname[nm]==prediction[nm] && lesname[nm]==1)
  {
    tp=tp+1
    
    
  }
  
  else if(lesname[nm]==prediction[nm] && lesname[nm]==-1)
  {
    tn=tn+1
    
    
  }
  
  else if(lesname[nm]==-1 && prediction[nm]==1)
  {
    fp=fp+1
    
    
  }
  
  else
  {
    fn=fn+1
    
    
  }
}
print(tp)
print(tn)
print(fp)
print(fn)

accuracy=((tp+tn)/(tp+tn+fn+fp))
accuracy
posacc=(tp/(tp+fp))
posacc
negacc=(tn/(tn+fn))
negacc
mcc=((tp*tn)-(fp*fn))/(sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn)))
mcc
feature_train[,1]
#seqtrain[distance[1]]
distance

  


