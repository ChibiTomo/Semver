package net.chibidevteam.semver.exceptions;

public class MalformedVersionException extends Exception {

    private static final long serialVersionUID = 1L;

    public MalformedVersionException(String msg, Exception e) {
        super(msg, e);
    }

    public MalformedVersionException(String msg) {
        super(msg);
    }

}
