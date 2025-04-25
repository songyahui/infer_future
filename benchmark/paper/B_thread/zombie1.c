//Alperen inci 
//18120205016 
// hw4 
// https://github.com/Alpereninci1/ThreadExercisesZombie/blob/master/zombie.c
 
#include <stdio.h>
#include <pthread.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>

static pthread_mutex_t mtx = PTHREAD_MUTEX_INITIALIZER;
/*number of zombies in the game: 
you should synchronize threads editing this variable*/
int zombieCounter;


/* Keeps track of number of zombies entered.*/
void zombieEntered(){
    
    zombieCounter++;
    
}
/* Keeps track of number of zombies killed.*/
void zombieKilled(){
   
   zombieCounter--;
   
}


/* Returns true if number of zombies in the room are 
greater than or equal to 100.*/
int tooManyZombiesInTheRoom(){
    if(zombieCounter>=100){

       return 1;
    }

   return 0;
}

/*Returns true if more than 100 zombies have been killed.*/
int killed100Zombies(){
    if(zombieCounter>100){

         return 1;
    }

    return 0;
   
}

/* Returns true if there is at least one zombies in the room.*/
int zombiesExist(){
    if(zombieCounter>=1){

        return 1;
    }
    return 0;
}
/*Returns the number of zombies killed.*/
int getKilledCount(){
     
     return zombieCounter;
}

/* Returns the number of zombies in the room.*/
int getInTheRoomCount(){
    
    return zombieCounter;
}
/*doorman thread*/
void *doorMan(void *p){

    //while(1){
    double x = ((double)rand()/RAND_MAX);
    pthread_mutex_lock(&mtx);
   
    if(tooManyZombiesInTheRoom() || killed100Zombies()){
       printf("odada kalan zombie sayisi = %d",getInTheRoomCount());
     exit(-1);       
    }
    else{
         if(x>=0.5 && x <=1.0)
         zombieEntered();
          
         sleep(0.002);
    }
   
    pthread_mutex_unlock(&mtx);

  //}
   
}

/*slayer thread*/
void *slayer(void *p){

 //while(1){

     pthread_mutex_lock(&mtx);

     if(zombiesExist()){

         zombieKilled();
         printf("oldurulen sayisi = %d \n",getKilledCount());    
         sleep(0.002);
     }
     else{
         
         exit(-1);
     }

     
     pthread_mutex_unlock(&mtx);
 //}
}
/*simulator main thread*/
int main(int argc, char **argv){
 
    int n = atoi(argv[1]);
    pthread_t t_doorMan[n]; 
    pthread_t t_slayer;
    for(int i=0;i<n;i++){
        
      int r1  = pthread_create(&t_doorMan[i] , NULL,doorMan,NULL);
      if(r1!=0){
          perror("thread creation");

          exit(EXIT_FAILURE);
      }
         
    }
    int r2= pthread_create(&t_slayer,NULL,slayer,NULL);
    if(r2!=0){

       perror("thread creation");
       exit(EXIT_FAILURE);
    }


    int r3 = pthread_join(*t_doorMan,NULL);
    if(r3!=0){
       
       perror("join");
       exit(EXIT_FAILURE);
    }

    int r4= pthread_join(t_slayer,NULL);
    if(r4!=0){
        perror("join");
        exit(EXIT_FAILURE);
    }
 
   // printf("oldurulen sayisi = %d",getKilledCount());
    // printf("odada kalan zombie sayisi = %d",getInTheRoomCount());

    return 0;

}