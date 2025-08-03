package com.mechanicalorchard.jclscan.model;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public final class ProcRef implements JclProc {
    private String name;
}