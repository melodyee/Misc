titles='''Visual Vibrometry: Estimating Material Properties From Small Motion in Video
Deeply Learned Attributes for Crowded Scene Understanding
How Many Bits Does it Take for a Stimulus to Be Salient?
Joint Inference of Groups, Events and Human Roles in Aerial Videos
Space-Time Tree Ensemble for Action Recognition
Modeling Video Evolution For Action Recognition
Fast and Flexible Convolutional Sparse Coding
Fast 2D Border Ownership Assignment
Situational Object Boundary Detection
Understanding Deep Image Representations by Inverting Them
Domain-Size Pooling in Local Descriptors: DSP-SIFT
Beyond Short Snippets: Deep Networks for Video Classification
segDeepM: Exploiting Segmentation and Context in Deep Neural Networks for Object Detection
The Treasure beneath Convolutional Layers: Cross-convolutional-layer Pooling for Image Classification
Superpixel-based Video Object Segmentation using Perceptual Organization and Location Prior
A Dynamic Convolutional Layer for Short Range Weather Prediction
Pedestrian Detection Aided by Deep Learning Semantic Tasks
Object Detection by Labeling Superpixels
Face Alignment by Coarse-to-Fine Shape Searching/
Transport-Based Single Frame Super Resolution of Very Low Resolution Face Images
Real-time part-based visual tracking via adaptive correlation filters
Fine-Grained Recognition Without Part Annotations
Fine-Grained Classification of Pedestrians in Video: Benchmark and State of the Art
Convolutional Neural Networks at Constrained Time Cost 
An Approximate Shading Model for Object Relighting
A Convolutional Neural Network Cascade for Face Detection
Curriculum Learning of Multiple Tasks
Joint Tracking and Segmentation of Multiple Targets'''
def get_title(x):
    for t in titles.split('\n'):
        if x.find(t)!=-1:
            return t
    return None
def get_url(ln):
    return "http"+ln.split('a href="http')[1].split('">[full')[0]
lns=[x for x in open('/tmp/cvpr15.htm').read().split('\n') if get_title(x) is not None]
with open('/tmp/t.htm','w') as f:
    for ln in lns:
        f.write(ln + '\n<br>')
        
print [(get_title(ln), get_url(ln)) for ln in lns]
import os
for ln in lns:
    os.system('wget {}'.format(get_url(ln)))

os.chdir('/tmp/')
for fn in glob.glob('*.pdf'):
    print fn
    #os.rename(fn,'CVPR 2015 {}'.format(fn))
