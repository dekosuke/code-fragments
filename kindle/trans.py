#!/usr/bin/python
# -*- coding: utf-8 -*-

import sys
import re

#from BeautifulSoup import BeautifulSoup, BeautifulStoneSoup 

def trans_html(html):
  p  = re.compile(r'<img.+?>', re.IGNORECASE|re.DOTALL)
  html = p.sub('', html)
  return html

if __name__ == '__main__':
  if len(sys.argv)<2:
    print 'usage (this) (target-url)'
    sys.exit()
  html = file(sys.argv[1],"r").read()
  print trans_html(html) 
