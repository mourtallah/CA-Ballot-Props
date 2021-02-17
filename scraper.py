from bs4 import BeautifulSoup
import requests
url="https://en.wikipedia.org/wiki/Category:2016_California_ballot_propositions"
page=requests.get(url)
soup=BeautifulSoup(page,"html_parser")
print(soup)
