method iter_copy<T(0)>(s: array<T>) returns (t: array<T>)
  ensures s.Length==t.Length
  ensures forall i::0<=i<s.Length ==> s[i]==t[i]
{
  t := new T[s.Length];
  var i:= 0;
  while (i < s.Length)
    invariant 0 <= i <= s.Length
    invariant forall x :: 0 <= x < i ==> s[x] == t[x]
  {
    t[i] := s[i];
    i:=i+1;
  }
}

method open(path: string, mode: int) returns (fd: int)
  ensures fd > 0 || fd == -1 
{
    if false {return -1;} 
    else {return 1;}
}



method iter_files(n: int, paths: array<string>) returns (fd: array<int>)
  requires 0 < n
  requires paths.Length >= n 
  ensures fd.Length ==n
  ensures forall j :: 0 <= j < n ==> fd[j] > 0 || fd[j] == -1
{
  fd := new int[n];

  var i:= 0;
  assert i == 0 && fd.Length ==n && 0 < n && paths.Length >= n;

  while (i < n)
  // Inv(i, n) <==> i <= n ; emp ; (_)^* 
  {
    i := i + 1; 
  }
  // i == 0 && fd.Length ==n && 0 < n && paths.Length >= n <: 0 <= n ; emp ; (_)^* 
  // i < n ; emp ; (_)^*   ==> i+1 <= n ; emp ; (_)^* 
  // i == n ; emp ; (_)^*  


  i:= 0;
  while (i < n)
  {
    fd[i] := open(paths[i], 0); // readonly
    i := i + 1; 
  }

  ex fd ; PredOpen(n, fd); PredFCOpen(n, fd);  

  forall i in [0, n). 
     i == n /\  emp  \/ 
     i < n /\ fd[i] >= 0 ; open (fd[i]) ; F close(fd[i])  \/ 
     i < n /\ fd[i] < 0 ; emp ; G(!fd[i])  
----------------------------------------------------
  forall i in [0, n). 
     i == n /\  emp    \/ 
     i < n /\ fd[i] >= 0 ; close (fd[i]) ; G(!fd[i])     \/ 
     i < n /\ fd[i] < 0 ; emp ; _^*
----------------------------------------------------
  forall i in [0, n). 
     i == n /\  emp    \/ 
     i < n /\ fd[i] >= 0 ; open (fd[i]).close (fd[i]) ; G(!fd[i])  \/ 
     i < n /\ fd[i] < 0 /\ emp ; G(!fd[i])  
     

  // 
  i == n  &&  forall j :: 0 <= j < n ==> 
    (fd[j] >= 0 ; open(fd[j]); F close(fd[j])) ||  fd[j] < 0  ; emp ; G(!fd[j]) 
  
  i == n  &&  forall j :: 0 <= j < n ==> 
    (fd[j] >= 0 ; open(fd[j]); F close(fd[j])) ||  fd[j] < 0  ; emp ; G(!fd[j]) 

Inv1(i, n, fd) == 
  i == n ; emp ; _^* \/ 
  i < n /\ fd[i] >= 0 ; open(fd[i]); F close(fd[i])  @ Inv1(i+1, n, fd) \/ 
  i < n /\ fd[i] < 0 ; emp ; G(!fd[i]) @ Inv1(i+1, n, fd) \/ 

Inv2(i, n, fd) == 
  i == n ; emp ; _^* \/ 
  i < n /\ fd[i] >= 0 ; close(fd[i]); G(!fd[i])  @ Inv2(i+1, n, fd) \/ 
  i < n /\ fd[i] < 0 ; emp ; _^* @ Inv2(i+1, n, fd) \/ 

fd[3] >= 0 ; close(fd[3]) ; 

Inv1(i, n, fd) == 
  i == n ; emp ; _^* \/ 
  i == 3 /\ fd[i] >= 0 ; open(fd[i]). close(fd[i]); G(!fd[i])  @ Inv1(i+1, n, fd) \/ 
  i < n /\ fd[i] >= 0 ; open(fd[i]); F close(fd[i])  @ Inv1(i+1, n, fd) \/ 
  i < n /\ fd[i] < 0 ; emp ; G(!fd[i]) @ Inv1(i+1, n, fd) \/ 


Inv3(i, n, fd) == 
Inv1(i, n, fd) @ Inv2(i, n, fd) == 
  i == n ; emp ; _^* \/ 
  i < n /\ fd[i] >= 0 ; open(fd[i]). close(fd[i]) ; G(!fd[i]) @ Inv3(i+1, n, fd)
  i < n /\ fd[i] < 0  ; emp ; G(!fd[i]) @ Inv3(i+1, n, fd)
}


// Inv(i, n, paths, fd)  == 
//     i == n ;  emp ; _^* ; () 
//     (i < n ; open(fd[i]); F free(fd[i]);) ;  Inv(i+1, n, paths, fd)

// i <= n /\ i < n ;  x \in [0 .. i] open(fd[x])  ;  x \in [0 .. i] F close(fd[x]) 
// i <= n /\ i >= n ;  x \in [0 .. i] open(fd[x])  ;  x \in [0 .. i] F close(fd[x]) 



