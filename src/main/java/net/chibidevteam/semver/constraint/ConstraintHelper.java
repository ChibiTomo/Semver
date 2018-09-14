package net.chibidevteam.semver.constraint;

import org.apache.commons.lang3.StringUtils;

import net.chibidevteam.semver.Version;
import net.chibidevteam.semver.exceptions.MalformedVersionException;
import net.chibidevteam.semver.serialization.Deserializer;

public class ConstraintHelper {

    private ConstraintHelper() {
    }

    public static boolean is(ConstraintSign constraint, String version) {
        for (String sign : constraint.getSigns()) {
            String regex = "^\\" + sign + "[\\d*xX].*$";
            if (StringUtils.isBlank(sign)) {
                regex = "^[\\d*xX].*$";
            }

            if (version.matches(regex)) {
                return true;
            }
        }
        return false;
    }

    public static boolean isEqualConstraint(String version) {
        return is(ConstraintSign.EQ, version);
    }

    public static boolean isNegationConstraint(String version) {
        return is(ConstraintSign.NOT, version);
    }

    public static boolean isLowerConstraint(String version) {
        return is(ConstraintSign.LT, version);
    }

    public static boolean isLowerOrEqualConstraint(String version) {
        return is(ConstraintSign.LTE, version);
    }

    public static boolean isGreaterConstraint(String version) {
        return is(ConstraintSign.GT, version);
    }

    public static boolean isGreaterOrEqualConstraint(String version) {
        return is(ConstraintSign.GTE, version);
    }

    public static boolean isCompatibilityConstraint(String version) {
        return is(ConstraintSign.COMPATIBLE, version);
    }

    public static boolean isNextSignificantConstraint(String version) {
        return is(ConstraintSign.NEXT_SIGNIFICANT, version);
    }

    public static boolean matches(String constraint, String version) throws MalformedVersionException {
        return matches(Deserializer.deserializeConstraint(constraint), Deserializer.deserializeVersion(version));
    }

    public static boolean matches(Constraint constraint, String version) throws MalformedVersionException {
        return matches(constraint, Deserializer.deserializeVersion(version));
    }

    public static boolean matches(String constraint, Version version) throws MalformedVersionException {
        return matches(Deserializer.deserializeConstraint(constraint), version);
    }

    public static boolean matches(Constraint constraint, Version version) {
        return constraint.matches(version);
    }
}
