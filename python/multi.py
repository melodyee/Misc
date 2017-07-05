import multiprocessing
import cv2
import time
import numpy as np

from multiprocessing import Process, Event


class Timer(Process):
    def __init__(self, interval, function, args=[], kwargs={}):
        super(Timer, self).__init__()
        self.interval = interval
        self.function = function
        self.args = args
        self.kwargs = kwargs
        self.finished = Event()

    def cancel(self):
        """Stop the timer if it hasn't finished yet"""
        self.finished.set()

    def run(self):
        while True:
            self.finished.wait(self.interval)
            if not self.finished.is_set():
                self.function(*self.args, **self.kwargs)
            #self.finished.set()

def f():
    print(time.time())

if __name__ == '__main__':
    t = Timer(1, f)
    t.run()
    print('Not printed')
