def bracketsParser(str, dict):
  ast = True
  pst = False

  print('another one:')
  cnvList = ()
  cnvDict = {}

  revList = ()

  i = 0

  for p in dict.items():
    cnvList += p
    revList += (i + 1,)

    for c in p:
      cnvDict[c] = i
      i += 1

  print(cnvList, cnvDict, revList)
  stt = []
  val = []
  pos = []

  for i in range(len(str)):
    if str[i] in cnvList:
      print(str[i], 'in cnvList')
      stt.append(ast)
      val.append(cnvDict[str[i]])
      pos.append(i)

  print(stt, val, pos)
  e = False

  for i in range(len(val)):
    if val[i] in revList:
      print(val[i], 'in revList')
      for j in range(i)[::-1]:
        if stt[j]:
          if val[i] == val[j] + 1:
            print(val[j], val[i], 'is pair, cross it')
            stt[i] = pst
            stt[j] = pst

            break
          else:
            print(val[j], val[i], 'is not pair. error')
            e = True
            break

      if e:
        break

  if not e:
    eps = -1

    for i, v in enumerate(stt):
      if v:
        eps = pos[i]
        break
  else:
    eps = pos[j]

  print('eps is', eps)
  return eps



  

dict = {
  '(': ')',
  '[': ']',
  '{': '}'
}

if __name__ == '__main__':
  assert bracketsParser('{}[]()',   dict) == -1, "test isn't passed"
  assert bracketsParser('{[(]}',    dict) ==  2, "test isn't passed"
  assert bracketsParser('[{}{}]()', dict) == -1, "test isn't passed"
  assert bracketsParser('[{}{}](}', dict) ==  6, "test isn't passed"
  assert bracketsParser('{[(',      dict) ==  0, "test isn't passed"
  assert bracketsParser('(}{)',     dict) ==  0, "test isn't passed"
  assert bracketsParser(')[',       dict) ==  0, "test isn't passed"
  assert bracketsParser(']{',       dict) ==  0, "test isn't passed"
  assert bracketsParser('([)]',     dict) ==  1, "test isn't passed"

  print("all tests are passed")