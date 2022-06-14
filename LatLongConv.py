##############################################################
#                                                             #
#                     IMPORT MODULES                          #
#                                                             #
###############################################################
import numpy as np
import pandas as pd
import sys
import pyproj
import os

###############################################################
#                                                             #
#                        CONSTANTS                            #
#                                                             #
###############################################################
from pyproj import transform

input_path = './processed/'
input_fname = 'Viedma1_040622'

# PataProj = pyproj.Proj(proj='utm', zone=18, ellps='WGS84')
PataProj = pyproj.Proj(init='epsg:32718')

# creating a list of all .csv files

csvFiles = []
for root, dirs, files in os.walk(input_path):
    for file in files:
        if file.endswith('.csv'):
            csvFiles.append(file)

print(csvFiles[0])
###############################################################
#                                                             #
#                      INPUT FIELDS                           #
#                                                             #
###############################################################

df = pd.read_csv(input_path+csvFiles[0])

EastNorth = df[["Easting", "Northing"]]

EastNorth = EastNorth.apply(lambda x: x.str.replace(',', '.'))

x = 654600.728765
y = 4526552.306845

lon, lat = PataProj(x, y, inverse=True)

print(lon)
print(lat)


def convertLL(row):
    easting = row['Easting']
    northing = row['Northing']

    lon, lat = PataProj(easting, northing, inverse=True)

    converted = transform(PataProj, PataProj, lon, lat)

    row['longitude'] = converted[0]
    row['latitude'] = converted[1]

    return row


EastNorth.transformed = EastNorth.apply(convertLL, axis=1)

print(EastNorth.transformed)

EastNorth.transformed.to_csv('/home/moritz/PycharmProjects/Moritz/cringe.csv', sep=',', header=True)


