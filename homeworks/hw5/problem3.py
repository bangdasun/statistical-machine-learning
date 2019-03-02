"""
Reference: http://machinelearningmastery.com/object-recognition-convolutional-neural-networks-keras-deep-learning-library/
"""

import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

os.chdir("C://Users//Bangda//Desktop//GR5241//hw5")

# Training and testing data
train = pd.read_table("zip_train.txt", sep = "", header = None)
train = np.array(train)
train = train[:, :257]
test = pd.read_table("zip_text.txt", sep = "", header = None)
test = np.array(test)

train_y = train[:, 0]
train_x = train[:, 1:]
test_y  = test[:, 0]
test_x  = test[:, 1:]
train_x1 = train_x[0].reshape(16, 16)
plt.imshow(train_x1, cmap=plt.get_cmap('gray'))

import numpy as np
from keras.datasets import cifar10
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import Dropout
from keras.layers import Flatten
from keras.constraints import maxnorm
from keras.optimizers import SGD
from keras.layers.convolutional import Conv2D
from keras.layers.convolutional import MaxPooling2D
from keras.utils import np_utils
from keras import backend as K
K.set_image_dim_ordering('th')

# Random seed
np.random.seed(1)

train_y = np_utils.to_categorical(train_y)
test_y  = np_utils.to_categorical(test_y)
num_classes = test_y.shape[1]
num_classes

train_x = train_x.reshape(train_x.shape[0], 1, 16, 16).astype('float32')
test_x = test_x.reshape(test_x.shape[0], 1, 16, 16).astype('float32')

def baseline_model():
    # create model
    model = Sequential()
    model.add(Conv2D(32, (5, 5), input_shape = (1, 16, 16), activation = 'relu'))
    model.add(MaxPooling2D(pool_size = (3, 3)))
    # model.add(Conv2D(16, (3, 3), activation = 'relu'))
    # model.add(MaxPooling2D(pool_size = (5, 5)))
    model.add(Dropout(0.2))
    model.add(Flatten())
    model.add(Dense(128, activation = 'relu'))
    # model.add(Dense(50, activation = 'relu'))
    model.add(Dense(num_classes, activation = 'softmax'))
    # Compile model
    model.compile(loss = 'categorical_crossentropy', optimizer = 'adam', metrics = ['accuracy'])
    return model
    
# build the model
model = baseline_model()
# Fit the model
model.fit(train_x, train_y, validation_data=(test_x, test_y), epochs = 30, batch_size = 300, verbose = 2)
# Final evaluation of the model
scores = model.evaluate(test_x, test_y, verbose = 0)
print("Baseline Error: %.2f%%" % (100 - scores[1] * 100))
