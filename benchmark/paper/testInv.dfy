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
  invariant i <= n 
  invariant forall j :: 0 <= j < i ==> fd[j] > 0 || fd[j] == -1
  //invariant forall j :: 0 <= j < i ==> fd[j] > 0  ; open(fd[j]); F close(fd[j]) 
  {
    fd[i] := open(paths[i], 0); // readonly
    i := i + 1; 
  }
  // forall j :: 0 <= j < n && fd[j] > 0 ==> open(fd[j]) ; F(close(fd[j])) 
  // forall j :: 0 <= j < n && fd[j] == -1 ==> emp ; G(!fd[j])

  //assert forall j :: 0 <= j < n ==> fd[j] > 0  ; open(fd[j]); F close(fd[j]) 

  assert forall j :: 0 <= j < n ==> fd[j] > 0 || fd[j] == -1; 
  assert fd.Length ==n; 
}


// Inv(i, n, paths, fd)  == 
//     i == n ;  emp ; _^* ; () 
//     (i < n ; open(fd[i]); F free(fd[i]);) ;  Inv(i+1, n, paths, fd)

// i <= n /\ i < n ;  x \in [0 .. i] open(fd[x])  ;  x \in [0 .. i] F close(fd[x]) 
// i <= n /\ i >= n ;  x \in [0 .. i] open(fd[x])  ;  x \in [0 .. i] F close(fd[x]) 



