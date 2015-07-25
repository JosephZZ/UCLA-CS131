import java.util.concurrent.locks.ReentrantLock;

class BetterSorryState implements State {
    private byte[] value;
    private byte maxval;
	private ReentrantLock lock;
	
	BetterSorryState(byte[] v) { value = v; maxval = 127; lock = new ReentrantLock();}

    BetterSorryState(byte[] v, byte m) { value = v; maxval = m; lock = new ReentrantLock();}

    public int size() { return value.length; }

    public byte[] current() { return value; }

    public boolean swap(int i, int j) {
	if (value[i] <= 0 || value[j] >= maxval) {
	    return false;
	}
	lock.lock();
	value[i]--;
	value[j]++;
	lock.unlock();
	return true;
    }
}
