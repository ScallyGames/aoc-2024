import re
import numpy as np
from matplotlib import pyplot as plt
from matplotlib import animation as animation
from scipy.stats import entropy
import pandas as pd

# fieldSize in (columns, rows)
fieldSize = (101, 103)

# fieldSize = (11, 7)

minEntropySoFar = 8.381

def get_entropy(state):
    fft = np.fft.fft2(state)
    power_spectrum = np.abs(fft)**2
    power_spectrum_flattened = power_spectrum.flatten()
    total_energy = np.sum(power_spectrum_flattened)
    probabilities = power_spectrum_flattened / total_energy
    # print(entropy(probabilities, base=None))
    # value, counts = np.unique(state, return_counts=True)
    return entropy(probabilities, base=None)

def has_low_entropy(state):
    global minEntropySoFar
    current_entropy = get_entropy(state)
    if current_entropy < minEntropySoFar:
        print(minEntropySoFar)
        minEntropySoFar = current_entropy
        return True
    else:
        return False
    # return current_entropy < 0.18


class AnimatedScatter(object):
    """An animated scatter plot using matplotlib.animations.FuncAnimation."""
    def __init__(self, numpoints=50):

        file = open("inputs/input14.txt", "r")
        content = file.read()
        file.close()

        numberGroupRegex = re.compile(r'(-?\d+)')

        robotsParams = [[int(matchResult) for matchResult in numberGroupRegex.findall(x)] for x in content.strip().split("\n")]

        self.current = pd.DataFrame(robotsParams)

        self.fig = plt.figure()
        self.ax = plt.axes()

        self.frame_count = 0
        print("Initializing")

        self.setup_plot()

        self.fig.canvas.mpl_connect('key_press_event', self.on_press)
        # self.ani = animation.FuncAnimation(self.fig, self.update,
        #                                   init_func=self.setup_plot, blit=True, interval=50)

    def on_press(self, event):
        self.img.remove()
        self.update()
        plt.show()

    def setup_plot(self):
        self.img = self.get_current_image()
        self.title = self.ax.text(50,100, "", bbox={'facecolor':'w', 'alpha':0.5, 'pad':5}, ha="center")

        return self.img, self.title


    def get_current_image_array(self):
        image = np.zeros(fieldSize)

        # localCopy = self.current.copy()
        # localCopy[0] = (localCopy[0] + 33) % fieldSize[0]
        # localCopy[1] = (localCopy[1] + 33) % fieldSize[1]
        # positionData = localCopy.iloc[:,:2].to_numpy()

        positionData = self.current.iloc[:,:2].to_numpy()
        image[tuple(positionData.T)] = 255
        return image

    def get_current_image(self):
        image = self.get_current_image_array()
        # fft = np.fft.fft2(image)

        # magnitude_spectrum = 20*np.log(np.abs(fftShifted))
        # return self.ax.imshow(magnitude_spectrum, cmap = 'gray')
        return self.ax.imshow(image)


    def update(self):
        i = 0
        # self.frame_count < 149300 or
        while (not has_low_entropy(self.get_current_image_array()) or i == 0):
            self.current[0] = (self.current[0] + self.current[2]) % fieldSize[0]
            self.current[1] = (self.current[1] + self.current[3]) % fieldSize[1]
            self.frame_count = self.frame_count + 1

            i += 1
            if self.frame_count % 100 == 0:
                print(str(self.frame_count))

        self.title.set_text(f"Frame: {self.frame_count}")
        self.img = self.get_current_image()

        # We need to return the updated artist for FuncAnimation to draw..
        # Note that it expects a sequence of artists, thus the trailing comma.
        return self.img, self.title


if __name__ == '__main__':
    a = AnimatedScatter()
    plt.show()
