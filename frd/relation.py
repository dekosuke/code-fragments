#/usr/bin/python

def read_csv(fname):
  f=file(fname, "r")
  lines = [a.strip() for a in f.readlines()]
  return [l.split(",") for l in lines]

def getcol(lines, i):
  return [l[i] for l in lines]

def correlation(ti, ri):
  pass

def timecorrelation(lines, i):
  cs = getcol(lines,i)
  s=0
  l=len(cs)-1
  for i in range(l):
     s*=cs[i]*cs[i+1]
  return float(s)/l


