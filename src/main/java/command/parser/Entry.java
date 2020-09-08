package outputGenerator.command.parser;

public class Entry{
    	final Double frequency;
    	final Double alpha;
    	final Double beta;
    	
    	public Entry(Double freq, Double alpha, Double beta) {
    		this.frequency = freq;
    		this.alpha = alpha;
    		this.beta = beta;
    	}

        public Double getFrequency() {
            return frequency;
        }

        public Double getAlpha() {
            return alpha;
        }

        public Double getBeta() {
            return beta;
        }

        public String toString() {
    		return "freq: " + frequency + ", alpha: " + alpha + ", beta: "+beta+"\n";
    	}
    }