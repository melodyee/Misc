import numpy as np
def phase_corr(img, img2):
  ''' 返回匹配程度（0到1）和img2 在img 中出现的位置'''
  assert img.shape[0] > img2.shape[0] and img.shape[1] > img2.shape[1]
  ga = np.fft.fft2(img)
  gb = np.fft.fft2(img2, s=img.shape)
  gagb = ga * np.conjugate(gb)
  r = np.fft.ifft2(gagb / np.abs(gagb))
  rval = np.abs(r).flatten().max()
  idx = np.abs(r).flatten().argmax()
  y = idx // r.shape[0]
  x = idx - y * r.shape[0]
  return rval, (x, y)
  
'''
# 算例
img = cv2.imread('/home/zsc/data/koala_mix/zsc.png', 0)[:200,:200]

In [37]: phase_corr(img, img[100:200, 100:200])
Out[37]: (0.21905914592411813, (100, 100))

In [38]: phase_corr(img, img[30:200, 30:200])
Out[38]: (0.7611746214230047, (30, 30))
'''
