package net.chibidevteam.semver;

import java.util.Arrays;
import java.util.List;

import net.chibidevteam.semver.exceptions.MalformedVersionException;

/**
 * @author THOMIAS Yannis
 */

public final class VersionHelper {

    private VersionHelper() {
    }

    public static String serialize(Version v) {
        return serialize(v.getMajor(), v.getMinor(), v.getPatch(), v.getPreRelease(), v.getMeta());
    }

    public static String serialize(Integer major) {
        return serialize(major, null, null, null);
    }

    public static String serialize(Integer major, Integer minor) {
        return serialize(major, minor, null, null);
    }

    public static String serialize(Integer major, Integer minor, Integer patch) {
        return serialize(major, minor, patch, null);
    }

    public static String serialize(Integer major, Integer minor, Integer patch, String preRelease, String... meta) {
        return serialize(major, minor, patch, preRelease, Arrays.asList(meta));
    }

    public static String serialize(Integer major, Integer minor, Integer patch, String preRelease, List<String> meta) {
        return Serializer.serialize(major, minor, patch, preRelease, meta);
    }

    public static Version deserialize(String str) throws MalformedVersionException {
        return Deserializer.deserialize(str);
    }
}
