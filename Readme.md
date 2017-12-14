# distributed-process-play

Test project to play around with the distributed process package.


# The Problem

Several nodes send messages of a random `Double` in the range `(0, 1]` to each
other. All nodes receive all messages, even ones originating from itself. The
test is divided into two epochs; during the first, nodes send messages and
during the second, messages sent in the first epoch continue to be received,
a calculation is performed and a result is printed. The lengths of the two
epochs are provided on the command line.

During the second epoch, each node prints out a tuple of
`(|m|, sum i .. |m| of i * m_i)`, where the messages are ordered by their send
time. Assuming the system clocks of all the nodes are synchronised and in the
absence of communication errors, all nodes should print out the same final tuple.



# Design Decisions and Assumptions

* The nodes need to sort the messages based on send time rather than the receive
  time. That means the message protocol needs to send both a time stamp and the
  random value in each message. It also means that in this exercise we have to
  assume that the system time on each node has already been synchronised to some
  global reference. In the absence of this assumption, the correct approach
  might be to do time synchronisation when the nodes first connect. One of the
  nodes would be chosen at random to be the reference and then some procedure
  would be used to synchronise that with the other nodes. I would read up on NTP
  (Network Time Protocol) as a starting point.

* I chose the `mwc-random` package for generating random numbers because it is
  supposed to be much faster than the original `random` package and I have used
  it before. If speed was really important I'd probably want to benchmark it
  against others.

* The scoring of the task ("The larger your score with a given sequence of random
  numbers is, the better.") and the way the sum part of the result is calculated
  suggests that speed is more important that correct-ness. If this was actually
  the case, I would probably use UDP for communication between the nodes rather
  than TCP. UDP would be possible because each message has to contain the
  timestamp at which it was sent so out of order UDP packets would not matter.
  UDP should also allow a higher throughput due to less handshaking resulting in
  a higher message count at each node and since the summation is weighted by the
  message index, a higher message count would result in a higher score. If using
  UDP it would also be wise to send a sequence number so that the receiving end
  could detect missing values and maybe even request that missing values be
  resent. However, given that this is a design/coding exercise its probably
  wiser to use TCP to get something working. If this was for real, a UDP
  implementation with re-sending of missing packets as necessary could be
  developed after the TCP implementation is working.

* Memory usage is an obvious concern. On a high speed network it would be
  possible to send a large number of messages in a very short amount of time and
  memory usage would rise as a product of the number of nodes and the number of
  messages per node. I will assume that these nodes are going to be run on
  machines with sufficient memory and will store the `(timestamp, random)` values
  in unboxed Vectors of unboxed values to minimise the memory requirements as
  much as possible. If this assumption was wrong, disk storage could be used.
  Messages could be received into Vectors in batches as before, sorted and then
  stored to disk already sorted. When the calculation needs to be done, the disk
  files can be pseudo-merged sorted and folded into a result so that the whole
  data set doesn't need to be in memory at once.

* Since I will be using unboxed vectors of unboxed values, the in-memory sorting
  should probably use one of the sorting algorithms in the `vector-algorithms`
  package. I've used the `Data.Vector.Algorithms.Tim` module for a similar
  problem in the past.

* During the second epoch, we need to wait for messages, do a calculation and
  print a result before the end of that epoch. Would be nice to know definitively
  if the sending node has finished. This suggests that there are two types of
  message, a message that holds the `(timestamp, random)` pair and a message
  that says there are no more messages. This special last message could also
  contain a count of how many messages were sent so that the receiver can
  validate the receive count. Also need to have a timeout so that if the
  terminating is not received, the process can still do a calculation on the
  values it has received and print that out.

