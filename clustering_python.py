## Author: Rajesh Jakhotia
## Company Name: K2 Analytics Finishing School Pvt. Ltd
## Email : ar.jakhotia@k2analytics.co.in
## Website : k2analytics.co.in

## Clustering for Retail Customer Spends data
## Hierarchical Clustering

## Let us first set the working directory path and import the data

## Install commands for packages
## Run from command prompt
## conda install package_name


import os
import pandas as pd
os.chdir("d:/K2Analytics/datafile")
os.getcwd()
RCDF = pd.read_csv("Cust_Spend_Data.csv")
RCDF

## Distance Computation
from scipy.spatial.distance import pdist, squareform
pdist ## Ctrl + i
help(pdist)
d_euc = pdist(RCDF.ix[:,2:7], metric  = "euclidean")


## Building the Clusters
from scipy.cluster.hierarchy import linkage, dendrogram, cut_tree
help(linkage)
clus1 = linkage(d_euc, method = "average")

## Displaying the Clusters in Dendrogram
import matplotlib.pyplot as plt
dendrogram(clus1, labels=RCDF[[1]].values.tolist())
plt.xlabel('hclust')
plt.ylabel('Distance')
plt.suptitle('Cluster Dendrogram', fontweight='bold', fontsize=14);


## Hierarchical Clustering with Scaling
from sklearn.preprocessing import scale as scale
## scale function standardizes the values
## Note - The scale function calculates the 
## Std. Dev assuming data is Sample
scaled_RCDF = scale(KRCDF.ix[:,2:7])
scaled_RCDF

## Note - Here we are scaling taking 
## the data as Population
scaled_RCDF = KRCDF.ix[:,2:7].apply(
        lambda x: (x- x.mean())/x.std())
scaled_RCDF

scaled_RCDF = scale(KRCDF.ix[:,2:7])
d_euc = pdist(scaled_RCDF, 
              metric  = "euclidean")
clus2 = linkage(d_euc, method = "average")

dendrogram(clus2, 
           labels=RCDF[[1]].values.tolist())
plt.xlabel('hclust')
plt.ylabel('Distance')
plt.suptitle('Cluster Dendrogram', 
             fontweight='bold', fontsize=14);

import numpy as np
print(np.round(squareform(d_euc).tolist(),3))

distance = clus2[:,2]
distance

## Profiling Step
RCDF['Clusters'] = cut_tree(clus2, 3)
clus_profile = RCDF.ix[:,2:8].groupby(['Clusters'], as_index=False).mean()
clus_profile 


## K Means Clustering

## Data Import & Scaling step
import os
import pandas as pd
os.chdir("d:/K2Analytics/datafile")
os.getcwd()
KRCDF = pd.read_csv("Cust_Spend_Data.csv")
KRCDF


from sklearn.preprocessing import scale as scale
scaled_RCDF = scale(KRCDF.ix[:,2:7])


## Identifying the optimal number of clusters 
# elbow method
cluster_range = range( 1, 6 )
cluster_wss = []

from sklearn.cluster import KMeans
for num_clusters in cluster_range:
  clusters = KMeans( num_clusters )
  clusters.fit(scaled_RCDF)
  cluster_wss.append( clusters.inertia_ )
  
from collections import OrderedDict
clusters_df = pd.DataFrame( OrderedDict ( 
        {"num_clusters": cluster_range, 
        "cluster_wss": cluster_wss }
        ) )
clusters_df[0:5]

import matplotlib.pyplot as plt
import numpy as np
plt.figure(figsize=(12,6))
plt.xlabel('# Clusters')
plt.ylabel('WSS')
plt.xticks(np.arange(min(clusters_df.num_clusters), 
                     max(clusters_df.num_clusters)+1, 
                     1.0))
plt.plot( clusters_df.num_clusters, 
         clusters_df.cluster_wss, 
         marker = "o" )

## profiling the clusters
help(KMeans)
clusterer = KMeans(n_clusters=3, random_state=10)
cluster_labels = clusterer.fit_predict(scaled_RCDF)
cluster_labels
KRCDF['Clusters'] = cluster_labels

clus_profile = KRCDF.ix[:,2:8].groupby(['Clusters'], 
                       as_index=False).mean()
clus_profile

from sklearn.decomposition import PCA
pca_2 = PCA(2)
plot_columns = pca_2.fit_transform(scaled_RCDF)


## Getting a Visual Plot 
## Defining Colours and Labels for the Plot
def color_fun (row):
   if (row['Clusters'] == 0):
      return "red"
   if (row['Clusters'] == 1):
      return "green"
   if (row['Clusters'] == 2):
      return "blue"
   return 0

KRCDF['color'] = KRCDF.apply (lambda row: color_fun(row), axis=1)

plot_labels = KRCDF[[1]].values.ravel()

## Show the Cluster Plot
plt.scatter(x=plot_columns[:,0], 
            y=plot_columns[:,1],
            c=KRCDF['color'].values.tolist(),
            s=50, edgecolors='none')

for label, x, y in zip(
        plot_labels, plot_columns[:,0], 
        plot_columns[:,1]) :
    plt.annotate(
    label,
    xy=(x, y), xytext=(10, 2),
    textcoords='offset points', ha='right', va='bottom',
    )
    plt.xlabel('PC1')
    plt.ylabel('PC2')

plt.show()
