package org.cse97b.json;
import java.io.*;
import com.eclipsesource.json.*;

public class ReadTweets {
   private  static final String rawtweetsfile = "shorttweets.txt";
   private static final String cookedtweetsfile = "cookedtweets.txt";
   private static final String field = "text";

   public static void main(String[] args) throws IOException {
       BufferedReader infile = new BufferedReader(new FileReader(rawtweetsfile));
       PrintWriter outfile = new PrintWriter(new File(cookedtweetsfile));
       String line = infile.readLine();
       String tweet = null;
       JsonObject json = null;

       while(line != null) {
           json = JsonObject.readFrom(line);
           tweet = json.get(field).asString();
           outfile.println(tweet);
           line = infile.readLine();
       }
       outfile.flush();
       outfile.close();
       infile.close();
   }

}