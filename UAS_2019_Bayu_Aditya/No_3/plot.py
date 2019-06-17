# Author : Bayu Aditya
# Copyright(c) 2019

import sys
import csv
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import axes3d

# Read CSV
csvFileName = sys.argv[1]
csvData = []
with open(csvFileName, 'r') as csvFile:
    csvReader = csv.reader(csvFile, delimiter=',')
    for csvRow in csvReader:
        csvData.append(csvRow)

# Get X, Y, Z
csvData = np.array(csvData)
csvData = csvData[1:,:].astype(np.float)
X, Y, Z = csvData[:,0], csvData[:,1], csvData[:,2]

# Plot X,Y,Z
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
ax.plot_trisurf(X, Y, Z, cmap = "jet", alpha=0.8)
ax.set_xlabel("X (Panjang) [Meter]")
ax.set_ylabel("Y (Waktu) [Detik]")
ax.set_zlabel("Z (Amplitudo) [Meter]")
plt.title("Grafik Problem No 3 UAS [2019]")
plt.show()