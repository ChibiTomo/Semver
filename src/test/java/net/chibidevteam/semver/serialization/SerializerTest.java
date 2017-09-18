package net.chibidevteam.semver.serialization;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;

import org.junit.Test;

public class SerializerTest {

    @Test
    public void serialize() {
        assertEquals("1", Serializer.serialize(1, null, null, null, null));
        assertEquals("1.2", Serializer.serialize(1, 2, null, null, null));
        assertEquals("1.2.5", Serializer.serialize(1, 2, 5, null, null));
        assertEquals("1.2.5-RC2", Serializer.serialize(1, 2, 5, "RC2", null));
        assertEquals("1.2.5-RC2+sha256", Serializer.serialize(1, 2, 5, "RC2", Arrays.asList("sha256")));
        assertEquals("1.2.5-RC2+sha256.2017", Serializer.serialize(1, 2, 5, "RC2", Arrays.asList("sha256", "2017")));

        assertEquals(".2.5-RC2+sha256.2017", Serializer.serialize(null, 2, 5, "RC2", Arrays.asList("sha256", "2017")));
        assertEquals("1.2.5-RC2+.2017", Serializer.serialize(1, 2, 5, "RC2", Arrays.asList(null, "2017")));
        assertEquals("-RC2+.2017", Serializer.serialize(null, null, null, "RC2", Arrays.asList(null, "2017")));
    }
}
