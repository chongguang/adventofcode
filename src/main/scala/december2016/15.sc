

def diskPosition(time: Int, original: Int, total: Int): Int = {
  (time + original)%total
}

diskPosition(2, 1, 2)

def disk1(time: Int) = diskPosition(time, 1, 17)
def disk2(time: Int) = diskPosition(time, 0, 7)
def disk3(time: Int) = diskPosition(time, 2, 19)
def disk4(time: Int) = diskPosition(time, 0, 5)
def disk5(time: Int) = diskPosition(time, 0, 3)
def disk6(time: Int) = diskPosition(time, 5, 13)
def disk7(time: Int) = diskPosition(time, 0, 11)

def findFirst(t: Int): Int = {
  if(
    disk1(t+1) == 0 &&
      disk2(t+2) == 0 &&
    disk3(t+3) == 0 &&
    disk4(t+4) == 0 &&
    disk5(t+5) == 0 &&
    disk6(t+6) == 0&&
      disk7(t+7) == 0
  ) t else findFirst(t+1)
}

findFirst(0)


/*
Disc #1 has 17 positions; at time=0, it is at position 1.
Disc #2 has 7 positions; at time=0, it is at position 0.
Disc #3 has 19 positions; at time=0, it is at position 2.
Disc #4 has 5 positions; at time=0, it is at position 0.
Disc #5 has 3 positions; at time=0, it is at position 0.
Disc #6 has 13 positions; at time=0, it is at position 5.
 */