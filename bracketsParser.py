def bracketsParser(str, dict):
  ast = True
  pst = False

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

  stt = []
  val = []
  pos = []

  for i in range(len(str)):
    if str[i] in cnvList:
      stt += [ast]
      val += [cnvDict[str[i]]]
      pos += [i]

  e = False

  for i in range(len(val)):
    if val[i] in revList:
      for j in range(i)[::-1]:
        if stt[j]:
          if val[i] == val[j] + 1:
            stt[i] = pst
            stt[j] = pst

            break
          else:
            e = True
            break

      if e:
        break

  if e:
    eps = pos[j]
  else:
    eps = -1

    for i, v in enumerate(stt):
      if v:
        eps = pos[i]
        break

  return eps

if __name__ == '__main__':
  dict = {
    '(': ')',
    '[': ']',
    '{': '}'
  }

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