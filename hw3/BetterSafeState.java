import java.util.concurrent.atomic.AtomicIntegerArray;

class BetterSafeState implements State {
    private AtomicIntegerArray value;
    private byte maxval;

    BetterSafeState(byte[] v) {
		maxval = 127; 
		value = new AtomicIntegerArray(v.length); 
		for (int i=0; i<v.length; i++) value.set(i,v[i]);
	}

    BetterSafeState(byte[] v, byte m) { 
		maxval = m; 
		value = new AtomicIntegerArray(v.length); 
		for (int i=0; i<v.length; i++) value.set(i,v[i]);
	}

    public int size() { return value.length(); }

    public byte[] current() { 
		byte[] cur_in_byte = new byte[value.length()];
		for (int i=0; i< value.length(); i++) cur_in_byte[i] = (byte) value.get(i);
		return cur_in_byte;
	}

    public boolean swap(int i, int j) {
	if (value.get(i) <= 0 || value.get(j) >= maxval) {
	    return false;
	}
	value.getAndDecrement(i);
	value.getAndIncrement(j);
	return true;
    }
}
