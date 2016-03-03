package csgo_demo_reader;

import java.nio.ByteBuffer;

public class BitBuffer {
    private static final int NORMAL_FRACTIONAL_BITS = 11;
    private static final int NORMAL_DENOMINATOR = (1 << NORMAL_FRACTIONAL_BITS) -1;
    private static final float NORMAL_RESOLUTION = (1.0f/NORMAL_DENOMINATOR);

    private static final int COORD_INTEGER_BITS = 14;
    private static final int COORD_FRACTIONAL_BITS = 3;
    private static final int COORD_DENOMINATOR =  1 << COORD_FRACTIONAL_BITS;
    private static final float COORD_RESOLUTION = 1.0f / COORD_DENOMINATOR;

    private static final int COORD_INTEGER_BITS_MP = 11;
    private static final int COORD_FRACTIONAL_BITS_MP_LOWPRECISION = 3;
    private static final int COORD_DENOMINATOR_LOWPRECISION =  1 << COORD_FRACTIONAL_BITS_MP_LOWPRECISION;
    private static final float COORD_RESOLUTION_LOWPRECISION = 1.0f / COORD_DENOMINATOR_LOWPRECISION;

    public enum CoordType {
        None, Integral, LowPrecision
    }
    private int curBits;
    private int bitsAvailable;
    private ByteBuffer byteBuffer;

    public BitBuffer(ByteBuffer byteBuffer) {
        this.byteBuffer = byteBuffer;
        curBits = 0;
        bitsAvailable = 0;
    }

    public long readSignedVarInt32() {
        return zigZagDecode(readVarInt32());
    }

    public long readSignedVarInt64() {
        return zigZagDecode(readVarInt64());
    }

    public float readBitNormal() {
        boolean negate = readBool();
        long fractValue = readBits(NORMAL_FRACTIONAL_BITS);
        float value = (float)fractValue * NORMAL_RESOLUTION;
        if (negate) {
            value = -value;
        }
        return value;
    }

    public float readBitCoordMP(CoordType coordType) {
        boolean isInBounds = readBool();
        boolean negate = false;
        float value = 0;
        if (coordType == CoordType.Integral) {
            boolean isIntValue = readBool();
            if (isIntValue) {
                negate = readBool();
                if (isInBounds) {
                    value = (float)(readBits(COORD_INTEGER_BITS_MP) + 1);
                } else {
                    value = (float)(readBits(COORD_INTEGER_BITS) + 1);
                }
            }
        } else {
            boolean isIntValue = readBool();
            negate = readBool();
            long intValue = 0;
            if (isIntValue) {
                if (isInBounds) {
                    intValue = readBits(COORD_INTEGER_BITS_MP) + 1;
                } else {
                    intValue = readBits(COORD_INTEGER_BITS) + 1;
                }
            }
            long fractValue = readBits(coordType == CoordType.LowPrecision ? COORD_FRACTIONAL_BITS_MP_LOWPRECISION : COORD_FRACTIONAL_BITS);
            value = intValue + ((float)fractValue * (coordType == CoordType.LowPrecision ? COORD_RESOLUTION_LOWPRECISION : COORD_RESOLUTION));
        }
        if (negate) {
            value = -value;
        }
        return value;
    }

    public float readBitCellCoord(int n, CoordType coordType) {
        float value;
        if (coordType == CoordType.Integral) {
            value = (float)readBits(n);
        } else {
            long intValue = readBits(n);
            long fractValue = readBits(coordType == CoordType.LowPrecision ? COORD_FRACTIONAL_BITS_MP_LOWPRECISION : COORD_FRACTIONAL_BITS);
            value = intValue + ((float)fractValue * (coordType == CoordType.LowPrecision ? COORD_RESOLUTION_LOWPRECISION : COORD_RESOLUTION));
        }
        return value;
    }

    public float readBitCoord() {
        float value = 0.0f;
        boolean hasIntVal = readBool();
        boolean hasFractVal = readBool();
        if (hasIntVal || hasFractVal) {
            boolean negate = readBool();
            long intVal = 0;
            long fractVal = 0;
            if (hasIntVal) {
                intVal = readBits(14) + 1;
            }
            if (hasFractVal) {
                fractVal = readBits(5);
            }
            value = intVal + ((float) fractVal * (1.0f/(1 << 5)));
            if (negate) {
                value = -value;
            }
        }
        return value;
    }

    public float readBitAngle(int n) {
        float shift = getBitMaskForBitnNum(n);
        float i = readBits(n);
        return i * (360.0f / shift);
    }

    public long readVarInt64() {
        long ret = 0;
        int count = 0;
        long b;

        do {
            if (count == 10) {
                return ret;
            }
            b = readBits(8);
            ret |= (b & 0x7F) << (7 * count);
            count += 1;
        } while((b & 0x80) !=0);
        return ret;
    }

    public long readVarInt32() {
        long ret = 0;
        int count = 0;
        long b;

        do {
            if (count == 5) {
                return ret;
            }
            b = readBits(8);
            ret |= (b & 0x7F) << (7 * count);
            count += 1;
        } while((b & 0x80) !=0);
        return ret;
    }

    public long readUnsignedBitVar() {
        long ret = readBits(6);
        long nBitFlag = ret & (16 | 32);
        int n = 0;
        if (nBitFlag == 16) {
            n = 4;
        } else if (nBitFlag == 32) {
            n = 8;
        } else if (nBitFlag == 48) {
            n = 28;
        }
        if (n != 0) {
            ret = (ret & 15) | (readBits(n) << 4);
        }
        return ret;
    }

    public float readBitFloat() {
        return Float.intBitsToFloat((int)readBits(32));
    }

    public boolean readBool() {
        return readBits(1) != 0;
    }

    public long readSignedBits(int n) {
        long ret = readBits(n);
        return (ret << (64 - n)) >> (64 - n);
    }

    public long readBits(int n) {
        while(n > bitsAvailable) {
            addByte();
        }
        int ret = curBits & getBitMask(n);
        bitsAvailable -= n;
        if (bitsAvailable == 0) {
            curBits = 0;
        } else {
            curBits >>= n;
        }
        return ret;
    }

    private long zigZagDecode(long v) {
        return (v >> 1) ^ - (v & 1);
    }

    private void addByte() {
        int newByte = 0xFF & byteBuffer.get();
        curBits = (newByte << bitsAvailable) | curBits;
        bitsAvailable += 8;
    }

    private static int getBitMask(int n) {
        return (1 << n) - 1;
    }

    private static int getBitMaskForBitnNum(int n) {
        return 1 << (n & 31);
    }
}
