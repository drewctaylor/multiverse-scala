 goal {
     strategy {
         find c1 c2 where
             c1 isnt c2
             c1 orientation contains c2 gender
             c2 orientation contains c1 gender
             relationship c1 c2 status does not contain marriage;
         subgoal alive c1;
         subgoal alive c2;
         subgoal single c1;
         subgoal single c2;
         insert marriage into c1 c2 status;
         narrate "{{c1.first.iterator.next}} and {{c2.first.iterator.next}} marry.";
     }
 }
 goal alive "Alive" {
     strategy c "Alive - No Op" {
         find c1 where
             c1 is c
             c1 life is dead;
         update c1 life to alive;
         narrate "{{c1.first.iterator.next}} returns to Melrose Place, very much alive.";
     }
     strategy c "Alive - Alive" {
         find c1 where
             c1 is c
             c1 life is alive;
         narrate "{{c1.first.iterator.next}} is alive.";
     }
 }
 goal single "Single" {
     strategy c "Single - Divorce" {
         find c1 c2 where
             c1 is c
             c1 isnt c2
             relationship c1 c2 status contains marriage;
         remove marriage from c1 c2 status;
         narrate "{{c1.first.iterator.next}} and {{c2.first.iterator.next}} divorce.";
     }
     strategy c "Single - Death" {
         find c1 c2 where
             c1 is c
             c1 isnt c2
             relationship c1 c2 status contains marriage;
         update c2 life to dead;
         remove marriage from c1 c2 status;
         narrate "{{c2.first.iterator.next}} dies, leaving {{c1.first.iterator.next}} alone.";
     }
     strategy c "Single - Noop" {
         none c1 c2 where
             c1 is c
             c1 isnt c2
             relationship c1 c2 status contains marriage;
         narrate "{{c.first.iterator.next}} is single.";
     }
}