package net.chibidevteam.semver.serialization;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;

import org.junit.Test;

import net.chibidevteam.semver.exceptions.MalformedVersionException;

public class SerializerTest {

    @Test
    public void serializeVersion() {
        assertEquals("1", Serializer.serializeVersion(1, null, null, null, null));
        assertEquals("1.2", Serializer.serializeVersion(1, 2, null, null, null));
        assertEquals("1.2.5", Serializer.serializeVersion(1, 2, 5, null, null));
        assertEquals("1.2.5-RC2", Serializer.serializeVersion(1, 2, 5, "RC2", null));
        assertEquals("1.2.5-RC2+sha256", Serializer.serializeVersion(1, 2, 5, "RC2", Arrays.asList("sha256")));
        assertEquals("1.2.5-RC2+sha256.2017",
                Serializer.serializeVersion(1, 2, 5, "RC2", Arrays.asList("sha256", "2017")));

        assertEquals(".2.5-RC2+sha256.2017",
                Serializer.serializeVersion(null, 2, 5, "RC2", Arrays.asList("sha256", "2017")));
        assertEquals("1.2.5-RC2+.2017", Serializer.serializeVersion(1, 2, 5, "RC2", Arrays.asList(null, "2017")));
        assertEquals("-RC2+.2017", Serializer.serializeVersion(null, null, null, "RC2", Arrays.asList(null, "2017")));
    }

    @Test
    public void serializeConstraint() throws MalformedVersionException {
        assertEquals("1", Serializer.serialize(Deserializer.deserializeConstraint("1")));
        assertEquals("!1.2-RC2", Serializer.serialize(Deserializer.deserializeConstraint("!1.2-RC2")));
        assertEquals(">1.5", Serializer.serialize(Deserializer.deserializeConstraint(">1.5")));
        assertEquals(">=1.6.2+sha256", Serializer.serialize(Deserializer.deserializeConstraint(">=1.6.2+sha256")));
        assertEquals("<1.5", Serializer.serialize(Deserializer.deserializeConstraint("<1.5")));
        assertEquals("<=1.6.2+sha256", Serializer.serialize(Deserializer.deserializeConstraint("<=1.6.2+sha256")));
        assertEquals("^1.9", Serializer.serialize(Deserializer.deserializeConstraint("^1.9")));
        assertEquals("~1.4.3", Serializer.serialize(Deserializer.deserializeConstraint("~1.4.3")));
        assertEquals(">2 !2.3 || ~1.5", Serializer.serialize(Deserializer.deserializeConstraint(">2 !2.3 || ~1.5")));
    }
}
