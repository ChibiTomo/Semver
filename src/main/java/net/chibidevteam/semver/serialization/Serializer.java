package net.chibidevteam.semver.serialization;

import static net.chibidevteam.semver.Constants.DELIMITER_META;
import static net.chibidevteam.semver.Constants.DELIMITER_PRE_RELEASE;
import static net.chibidevteam.semver.Constants.SEPARATOR_META;
import static net.chibidevteam.semver.Constants.SEPARATOR_VERSION;

import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

public class Serializer {

    private Serializer() {
    }

    public static String serialize(Integer major, Integer minor, Integer patch, String preRelease, List<String> meta) {
        StringBuilder sb = new StringBuilder();
        if (major != null) {
            sb.append(major);
        }
        if (minor != null) {
            sb.append(SEPARATOR_VERSION);
            sb.append(minor);
        }
        if (patch != null) {
            sb.append(SEPARATOR_VERSION);
            sb.append(patch);
        }
        if (preRelease != null) {
            sb.append(DELIMITER_PRE_RELEASE);
            sb.append(preRelease);
        }
        if (!CollectionUtils.isEmpty(meta)) {
            sb.append(DELIMITER_META);
            sb.append(StringUtils.join(meta, SEPARATOR_META));
        }

        return sb.toString();
    }
}
