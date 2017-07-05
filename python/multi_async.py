#!/usr/bin/env python3
import multiprocessing
import cv2
import time
import numpy as np

from multiprocessing import Process, Event

def f():
    e = Event()
    while True:
        e.wait(1)
        print(time.time())

if __name__ == '__main__':
    p = Process(target=f, args=())
    p.start()
    print('Here')
    p.join()
    print('Not printed')
