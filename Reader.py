import os,random

def getrandfromMem(filename) :
  fd = file(filename,'rb')
  l = fd.readlines()
  pos = random.randint(0,len(l))
  fd.close()
  return (pos,l[pos])

def getrandomline2(filename) :
  filesize = os.stat(filename)[6]
  if filesize < 4096 :  # Seek may not be very useful
    return getrandfromMem(filename)

  fd = file(filename,'rb')
  for _ in range(10) : # Try 10 times
    pos = random.randint(0,filesize)
    fd.seek(pos)
    fd.readline()  # Read and ignore
    line = fd.readline()
    if line != '' :
       break

  if line != '' :
    return (pos,line)
  else :
    getrandfromMem(filename)

print getrandomline2("en_US.twitter.txt")