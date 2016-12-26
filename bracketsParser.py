def bracketsParser(str, dict):
  ast = True
  dst = False

  trgList = tuple(dict.keys()) + tuple(dict.values())
  data = [[ast, v, i] for i, v in enumerate(str) if v in trgList]

  revList = tuple(dict[k] for k in dict)
  revDict = {dict[k]:k for k in dict}

  e = False

  print('run main loop, steps:', range(len(data)))
  for i in range(len(data)):
    m = data[i]
    print('m is', m)

    if m[1] in revList:
      print(m[1], 'in revList')
      print('loop for', range(i)[::-1])

      for j in range(i)[::-1]:
        print('j is', j)
        p = data[j]

        if p[0]:
          if p[1] == revDict[m[1]]:
            print('found a pair, disact')
            data[i][0] = dst
            data[j][0] = dst

            break
          else:
            e = True

        if e:
          break

      if e:
        break

  print('data is', data)

  if not e:
    i = -1

    for p in data:
      if p[0]:
        print('odd bracket!')
        i = p[2]
        break

    return i
  else:
    print('invalid!')
    return p[2]

dict = {
  '(': ')',
  '[': ']',
  '{': '}'
}

print(bracketsParser("{[}]}", dict))

assert bracketsParser('{}[]()',   dict) == -1, "test isn't passed"
assert bracketsParser('{[(]}',    dict) ==  2, "test isn't passed"
assert bracketsParser('[{}{}]()', dict) == -1, "test isn't passed"
assert bracketsParser('[{}{}](}', dict) ==  6, "test isn't passed"
assert bracketsParser('{[(',      dict) ==  0, "test isn't passed"
assert bracketsParser('(}{)',     dict) ==  0, "test isn't passed"
assert bracketsParser(')[',       dict) ==  0, "test isn't passed"
assert bracketsParser(']{',       dict) ==  0, "test isn't passed"
assert bracketsParser('([)]',     dict) ==  1, "test isn't passed"