import java.util.concurrent.locks.*;

class TrainMonitor {

	private final Lock lock = new ReentrantLock();
	private final Condition cond = lock.newCondition();
	private boolean isClear = true;

	public void enter() throws InterruptedException {
		lock.lock();
		try {
			while(!isClear) {
				cond.await();
			}
			isClear = true;
		}finally {
			lock.unlock();

		System.out.println("enter");
		}
	}

	public void leave() {
		lock.lock();
		isClear = true;
		System.out.println("lämnar");
		cond.signal();
		lock.unlock();
	}

	public boolean tryEnter() throws InterruptedException {
		lock.lock();
		System.out.println(" & " + isClear);
		if(isClear) {		
			isClear = false; 
			lock.unlock();
			return true;
		}
		else {
			lock.unlock();
			return false;
		}
	}
	public boolean getStatus() {
		return isClear;
	}


}