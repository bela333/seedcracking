import java.util.Random;

public class example {

  private static final long multiplier = 0x5DEECE66DL;
  private static final long addend = 0xBL;
  private static final long mask = (1L << 48) - 1;

  public static void main(String args[]) {
    Random rand = new Random();
    long chunkX = 7;
    long chunkZ = 16;

    long seed = 123123123123123L;
    long seed0 = (seed ^ multiplier); // scrambled seed
    long seed1 = (seed0 * multiplier + addend) & mask;
    long seed2 = (seed1 * multiplier + addend) & mask;
    long seed3 = (seed2 * multiplier + addend) & mask;
    long seed4 = (seed3 * multiplier + addend) & mask;

    // casting to int makes sure that sign bits are extended
    // Also, in Java integer division rounds towards 0
    long var7_ = (((seed1 >>> 16) << 32) + (int) (seed2 >>> 16));
    long var7 = var7_ | 1;
    if (var7_ < 0 && (var7_ & 1) == 1) {
      System.out.println("FUCK");
      var7 += 2;
    }
    long var9_ = (((seed3 >>> 16) << 32) + (int) (seed4 >>> 16));
    long var9 = var9_ | 1;
    if (var9_ < 0 && (var9_ & 1) == 1) {
      System.out.println("FUCK");
      var9 += 2;
    }
    long chunkseed = (chunkX * var7 + (chunkZ * var9) ^ seed) & mask;

    rand.setSeed(seed);
    long randx = rand.nextLong() / 2L * 2L + 1L;
    long randz = rand.nextLong() / 2L * 2L + 1L;
    long realchunkseed = ((long) chunkX * randx + (long) chunkZ * randz ^ seed) & mask;

    System.out.println(chunkseed + " " + realchunkseed + " " + (chunkseed == realchunkseed));
    System.out.println(chunkX * var7);
    System.out.println(chunkseed);

  }
}
