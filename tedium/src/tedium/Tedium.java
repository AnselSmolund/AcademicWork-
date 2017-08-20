
package tedium;
import java.util.Random;

//  CARD. A playing card. It's immutable.

final class Card
{

//  RANK NAME. Printable names of card ranks.

  private static final String [] rankName =
   {
     "ace",     //   0
     "two",     //   1
     "three",   //   2
     "four",    //   3
     "five",    //   4
     "six",     //   5
     "seven",   //   6
     "eight",   //   7
     "nine",    //   8
     "ten",     //   9
     "jack",    //  10
     "queen",   //  11
     "king"     //  12
   };

//  SUIT NAME. Printable names of card suits.

  private static final String [] suitName =
   {
     "spade",   //  0
     "heart",   //  1
     "diamond", //  2
     "club"     //  3
   };

  private int rank;  //  Card rank, between 0 and 12 inclusive.
  private int suit;  //  Card suit, between 0 and  3 inclusive.
  public Card(int rank, int suit)
  {
    if (0 <= suit && suit <= 3 && 0 <= rank && rank <= 12)
    {
      this.rank = rank;
      this.suit = suit;
    }
    else
    {
      throw new IllegalArgumentException("No such card.");
    }
  }
  public int getRank()
  {
    return rank;
  }
  
  public int getSuit()
  {
    return suit;
  }

  public String toString()
  {
    return "the " + rankName[rank] + " of " + suitName[suit] + "s";
  }
}

class Deck{
    int deckCount = 52; 
    public Card[] deck;
    Random rand = new Random();
    public Deck(){
        deck = new Card[52];    //Array containing 52 elements 
        int cardCount = 0;
        for(int i = 0; i <= 3; i++){
            for(int j = 0; j <= 12; j++){
                deck[cardCount] = new Card(j,i);    // Assigning 52 different cards 
                cardCount++;
            }
        }
    }
    public void shuffle(){
        int j;
        if(deckCount == 52){        //make sure no cards have been dealt yet
            for(int i = deck.length-1; i > 0; i--){
                j = Math.abs(rand.nextInt()) % (i);     //random integer between 0 and i
                Card a = deck[j];            //temporary card set to random card in deck
                deck[j] = deck[i];          // set deck at random integer to deck at i 
                deck[i] = a;                //set deck at i to the temporary card
            }
        }
        else{
            throw new IllegalStateException("Cards have been dealt, cannot shuffle");
        }
        
    }
    
    public boolean canDeal(){
        return deckCount > 0;  //check if there are more than 0 cards in the deck
    }
    public Card deal(){
        if(canDeal()){          //if there are cards left to deal
           deckCount--;         //subtract one from deck count
           return deck[deckCount];  //return deck at current deckcount 
        }  
        else{
            throw new IllegalStateException("No more cards to be dealt");
        }
    }
}
class Tableau{
    private class Pile{          //nested class
        private Card card;          //private slot card
        private final Pile next;    //private slot next 
        private Pile(Card card, Pile next){     //constructor 
            this.card = card;
            this.next = next; 
        }
    }
    private Pile top; //top of stack
    
   
    public Tableau(){
        
        top = null;     //new empty instance of tableau 
    }
    public void addPile(Card card){
        top = new Pile(card,top);       //new card to the top of the stack
        System.out.println("Added " + top.card);   
    }
    
    private boolean canMerge(){
        if(hasManyPiles()){
            return canPutOn(top.next.card, top.card);   //test if next and top card can merge 
        }
        else{
            return false;
        }
    }
    
    private boolean canPutOn(Card left, Card right){
        return left.getRank() < right.getRank()|| left.getSuit() == right.getSuit();
    }
    private boolean hasManyPiles(){
        return top.next!=null;
    }
    
    private void mergeTwoPiles(){
            
            System.out.println("Merged " + top.next.card + " and the " + top.card);
            
            top.next.card = top.card;
            top = top.next;
           
    }
    private void results(){
        if(top.next == null){
            System.out.println("You won the game");
        }
        else{
            System.out.println("You lost the game");
        }
    }
    public void play(){
        
        Deck deck = new Deck();
        deck.shuffle();
        addPile(deck.deal());
        while(deck.canDeal()){
            addPile(deck.deal());
            while(canMerge()){
                mergeTwoPiles();
            }
      
        }
        results();
        
    }
    
    
}
public class Tedium {

    public static void main(String[] args) {
       Tableau p = new Tableau();
       p.play();
      
    }    
}
