import poplib
import re

popClient = poplib.POP3('192.168.1.34')

popClient.user('user')
popClient.pass_('password')

numMsgs, mboxSize = popClient.stat()

print ("Number of messages ", numMsgs)
print ("Mailbox size", mboxSize)
print

for id in range (numMsgs):
  for mail in popClient.retr(id+1)[1]:
    if re.search( 'Subject:', mail ):
      print (mail)

  print


popClient.quit()
