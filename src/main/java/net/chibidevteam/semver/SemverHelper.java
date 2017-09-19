package net.chibidevteam.semver;

import java.util.Arrays;
import java.util.List;

import net.chibidevteam.semver.exceptions.MalformedVersionException;
import net.chibidevteam.semver.serialization.Deserializer;
import net.chibidevteam.semver.serialization.Serializer;

/**
 * @author THOMIAS Yannis
 */

public final class SemverHelper {

    private SemverHelper() {
    }

    public static String serializeVersion(Version v) {
        return serializeVersion(v.getMajor(), v.getMinor(), v.getPatch(), v.getPreRelease(), v.getMeta());
    }

    public static String serializeVersion(Integer major) {
        return serializeVersion(major, null, null, null);
    }

    public static String serializeVersion(Integer major, Integer minor) {
        return serializeVersion(major, minor, null, null);
    }

    public static String serializeVersion(Integer major, Integer minor, Integer patch) {
        return serializeVersion(major, minor, patch, null);
    }

    public static String serializeVersion(Integer major, Integer minor, Integer patch, String preRelease,
            String... meta) {
        return serializeVersion(major, minor, patch, preRelease, Arrays.asList(meta));
    }

    public static String serializeVersion(Integer major, Integer minor, Integer patch, String preRelease,
            List<String> meta) {
        return Serializer.serializeVersion(major, minor, patch, preRelease, meta);
    }

    public static Version deserializeVersion(String str) throws MalformedVersionException {
        return Deserializer.deserializeVersion(str);
    }
}
