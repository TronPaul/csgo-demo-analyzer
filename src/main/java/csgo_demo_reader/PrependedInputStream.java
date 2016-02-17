package csgo_demo_reader;

import java.io.IOException;
import java.io.InputStream;

public class PrependedInputStream extends InputStream {
    private int prependPosition = 0;
    private byte[] prependedBytes;
    private InputStream wrappedStream;

    public PrependedInputStream(byte[] prependedBytes, InputStream wrappedStream) {
        this.prependedBytes = prependedBytes;
        this.wrappedStream = wrappedStream;
    }

    @Override
    public int read() throws IOException {
        if (prependedBytes == null) {
            throw new IOException("Stream closed");
        }

        int ret;
        if (prependPosition < prependedBytes.length) {
            ret = prependedBytes[prependPosition];
            prependPosition += 1;
        } else {
            ret = wrappedStream.read();
        }
        return ret;
    }

    @Override
    public void close() throws IOException {
        wrappedStream = null;
        prependedBytes = null;
        prependPosition = 0;
    }
}
