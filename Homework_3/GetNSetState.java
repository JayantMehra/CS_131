import java.util.concurrent.atomic.AtomicIntegerArray;

public class GetNSetState implements State{
	private AtomicIntegerArray value;
	private byte maxval;

	GetNSetState(byte[] val){
		int[] v = new int[val.length];
		for(int i = 0; i < val.length; i++){
			v[i] = val[i];
		}
		value = new AtomicIntegerArray(v);
		maxval = 127;
	}

	GetNSetState(byte[] val, byte max){
		int[] v = new int[val.length];
		for(int i = 0; i < val.length; i++){
			v[i] = val[i];
		}
		value = new AtomicIntegerArray(v);
		maxval = max;
	}

	public int size(){
		return value.length();
	}

	public byte[] current(){
		byte[] v = new byte[value.length()];
		for(int i = 0; i < value.length(); i++){
			v[i] = (byte) value.get(i);
		}
		return v;
	}

	public boolean swap(int i, int j)
	{
	    if(value.get(i) <= 0 || value.get(j) >= maxval)
	        return false;
	    value.set(i, value.get(i) - 1);
	    value.set(j, value.get(j) + 1);
	    return true;
	}

}