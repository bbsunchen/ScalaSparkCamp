package org.cse97b.json;
import java.io.IOException;
import java.util.*;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.conf.*;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.*;
import org.apache.hadoop.mapreduce.lib.input.*;
import org.apache.hadoop.mapreduce.lib.output.*;
import org.apache.hadoop.util.*;
import com.eclipsesource.json.*;
	
public class TopWords {
	static enum MyExceptions {IO_EXCEPTION, INTERRUPTED_EXCEPTION, NULL_POINTER_EXCEPTION}

	public static class Map extends Mapper<LongWritable, Text, Text, IntWritable> {
		private final static IntWritable one = new IntWritable(1);
		private Text word = new Text(); // for output formation
		private final static String field = "text";
		JsonObject json = null; //josn is used for analyzing tweet
 	
		protected void setup(Context context) throws IOException, InterruptedException {

		}
		protected void cleanup(Context context) throws IOException, InterruptedException {
     
		} 

		protected void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
			context.progress(); //in case of long running code, report that something is happening
			/*
				For each input line, Json transfer it to tweets.
				For each tweets, split it into words.
				For each word, map emit (word, 1).
			*/
			String line = value.toString();
			json = JsonObject.readFrom(line);
			String tweet = json.get(field).asString();
			String[] list = tweet.split("[^A-Za-z]+");	// split tweets by non-word character		
			for(String s : list){
				if (s.length() != 0){ //throw away word that is blank
					String result = s.toLowerCase(); // transfer all words into lower case
					word.set(result);
					context.write(word, one); // emit
				}
			}
		}
	}

	public static class Reduce extends Reducer<Text, IntWritable, Text, IntWritable> {
		private int K = 0; // parameter from command line, reducer will output the first K most frequent word
		private Text word = new Text();
		
		/*
			For reducer, we maintain the first K most frequent word and their frequency.
			K words & corresponding frequency will be in order.
		*/
		private String[] wordsArray; // maintian the first K most frequent words
		private int[] timesArray;    // maintian frequencies of the first K most frequent words
		
		protected void setup(Context context) throws IOException, InterruptedException {
			/*get K value from command line*/
			Configuration conf = context.getConfiguration();
			K = conf.getInt("k", 50); // default value of K is 50
			
			/*initialize wordsArray and timesArray*/
			wordsArray = new String[K];
			timesArray = new int[K];
			for(int i = 0; i < K; i++){
				timesArray[i] = 0;
			}
		}
		protected void cleanup(Context context) throws IOException, InterruptedException {
			/*emit first K words and their occurance*/
			for(int i = 0; i < K; i++){
				word.set(wordsArray[i]);
				context.write(word, new IntWritable(timesArray[i]));
			}
		}
		
		protected void reduce(Text key, Iterable<IntWritable> values, Context context) throws IOException, InterruptedException {
			/*
			 * key is the current word
			 * sum will be the current word's frequency
			*/
			int sum = 0; 
			context.progress(); //in case of long running code, report that something is happening
			for(IntWritable val: values) {
				sum += val.get();
			}
			for(int i = 0; i < K; i++){
				if(sum > timesArray[i]){
					/*
						If sum is greater than i-th word's frequency in array,
						move words and their corresponding frequency behind i-th for one step,
						for instance, word & frequency in j-th(i < j < K) position will be in (j+1)-th position
						then insert sum and current word into i-th position.
					*/
					for(int j = K-1; j > i; j--){
						timesArray[j] = timesArray[j-1];
						wordsArray[j] = wordsArray[j-1];
					}
					timesArray[i] = sum;
					wordsArray[i] = key.toString();
					break;
				}
			}
		}
	}

	private static boolean deleteOutputDir(Job job, Path p) throws IOException {
		/*
			If path p exist, delete it.
		*/
		boolean retvalue = false;
		Configuration conf = job.getConfiguration();
		FileSystem myfs = p.getFileSystem(conf);
		if(myfs.exists(p) && myfs.isDirectory(p)) {
			retvalue = myfs.delete(p,true);
		}
		return retvalue;
	}

	public static void main(String[] args) throws Exception {
		Configuration conf = new Configuration();
		/*communicating k value to mapreduce*/
		int k = new Integer(args[2]);
		conf.setInt("k", k);

		Job job = Job.getInstance(conf); // pass value of k to mapreduce
		
		job.setJarByClass(TopWords.class);
		job.setJobName("topwords");

		/* type of map output */
		job.setMapOutputKeyClass(Text.class);
		job.setMapOutputValueClass(IntWritable.class);	

		/* type of reduce output */
		job.setOutputKeyClass(Text.class);
		job.setOutputValueClass(IntWritable.class);

		/* specify input/output directories */
		FileInputFormat.setInputPaths(job, new Path(args[0]));
		deleteOutputDir(job,new Path(args[1]));
		FileOutputFormat.setOutputPath(job, new Path(args[1]));

		/* How to read and write inputs/outputs */
		job.setInputFormatClass(TextInputFormat.class);
		job.setOutputFormatClass(TextOutputFormat.class);

		/* specify program components */
		job.setMapperClass(Map.class);
		job.setReducerClass(Reduce.class);

		/* number of reducers */
		job.setNumReduceTasks(1);     

		/* More advanced components */
		//job.setCombinerClass(MyCombiner.class);
		//job.setSortComparatorClass(MyComparator.class);
		//job.setPartitionerClass(MyPartitioner.class);

		/* run job */
		boolean result = job.waitForCompletion(true);

		/* access counters */
		Counters counters = job.getCounters();
		Counter acounter = counters.findCounter(MyExceptions.IO_EXCEPTION);
		long iocount = acounter.getValue();
		System.exit(result?0:1);
	}
}